#include <llvm/IR/NoFolder.h>
#include <obf0/passes/opq/opq.hpp>

#include <obf0/passes/mba/mba.hpp>
#include <obf0/utility/llvm.hpp>

#include <cstdlib>
#include <random>
#include <vector>

#include <llvm/ADT/STLExtras.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Use.h>
#include <llvm/Support/Casting.h>
#include <llvm/Transforms/Utils/Cloning.h>
#include <llvm/Transforms/Utils/SSAUpdater.h>

namespace obf0::opq
{
	template<typename T>
	static llvm::Value* make_opaque_value( llvm::IRBuilder<T>& builder, llvm::Value* v, llvm::AllocaInst* storage )
	{
		builder.CreateStore( v, storage, true );
		llvm::LoadInst* volatile_val = builder.CreateLoad( v->getType(), storage, true );
		return volatile_val;
	}

	bool visit_opq( llvm::Function& f )
	{
		llvm::errs() << "(obf0-opq) obfuscating " << f.getName() << "\n";

		std::mt19937 random_generator( std::random_device{}() );

		// Pick a random block for the opaque "not target"
		auto random_block = [ & ]( std::vector<llvm::BasicBlock*> a ) {
			std::uniform_int_distribution<std::size_t> distribution( 0, a.size() - 1 );
			int idx = distribution( random_generator );
			return a[ idx ];
		};
		std::vector<llvm::BasicBlock*> worklist;
		for ( llvm::BasicBlock& head_bb : f )
		{
			// Skip no return, EH pad, or first block
			if ( llvm::isa<llvm::InvokeInst>( head_bb.getTerminator() ) || head_bb.isEHPad() || head_bb.isEntryBlock() )
				continue;
			worklist.emplace_back( &head_bb );
		}

		llvm::errs() << "(obf0-opq)\tobfuscating " << worklist.size() << " blocks\n";
		if ( worklist.empty() )
			return false;

		llvm::IRBuilder<> entry_builder( &*f.getEntryBlock().getFirstInsertionPt() );
		llvm::AllocaInst* opaque_i32 = entry_builder.CreateAlloca( entry_builder.getInt32Ty(), nullptr, "opq.i32" );
		llvm::IRBuilder<> builder( &*f.getEntryBlock().getFirstInsertionPt() );

		auto make_pred = [ & ]() -> llvm::Value* {
			llvm::Value* seed  = make_opaque_value( builder, builder.getInt32( random_generator() ), opaque_i32 );
			auto positive_byte = [ & ]( unsigned shift ) {
				llvm::Value* shifted = shift == 0 ? seed : builder.CreateLShr( seed, builder.getInt32( shift ) );
				llvm::Value* byte    = builder.CreateAnd( shifted, builder.getInt32( 0xff ) );
				return builder.CreateZExt( builder.CreateAdd( byte, builder.getInt32( 1 ) ), builder.getInt64Ty() );
			};
			auto cube      = [ & ]( llvm::Value* value ) { return builder.CreateMul( builder.CreateMul( value, value ), value ); };
			llvm::Value* a = positive_byte( 0 );
			llvm::Value* b = positive_byte( 8 );
			llvm::Value* c = positive_byte( 16 );
			return builder.CreateICmpNE( builder.CreateAdd( cube( a ), cube( b ) ), cube( c ) );
		};

		for ( llvm::BasicBlock* head_bb : worklist )
		{
			// Split it into head -> body -> tail
			// Head: phi nodes, debug, etc.
			// Body: bulk of the block
			// Tail: last instruction
			llvm::BasicBlock* body_bb = head_bb->splitBasicBlock( head_bb->getFirstNonPHIOrDbgOrLifetime() );
			llvm::BasicBlock* tail_bb = body_bb->splitBasicBlock( body_bb->getTerminator() );

			// Clone body
			llvm::ValueToValueMapTy v_map;
			llvm::BasicBlock* body_clone_bb = CloneBasicBlock( body_bb, v_map, "", body_bb->getParent() );
			llvm::SmallVector<llvm::BasicBlock*, 1> blocks{ body_clone_bb };
			remapInstructionsInBlocks( blocks, v_map );

			// 1) Head and body used to be part of the same block, now they will be joined by an OP
			head_bb->getTerminator()->eraseFromParent();
			// 2) Tail is the last instruction of body, thus body must be reconnected to tail by an OP
			body_bb->getTerminator()->eraseFromParent();
			// 3) Body clone is a decoy which should also be attached to head
			body_clone_bb->getTerminator()->eraseFromParent();

			// 1) Create an opaque jump from head -> body
			builder.SetInsertPoint( head_bb );
			builder.CreateCondBr( make_pred(), body_bb, body_clone_bb );

			// 2) Create an opaque jump from body -> tail
			// Note: headBB can't be a target of the False condition
			builder.SetInsertPoint( body_bb );
			builder.CreateCondBr( make_pred(), tail_bb, random_block( { body_bb, body_clone_bb, tail_bb } ) );

			builder.SetInsertPoint( body_clone_bb );
			// The cloned block gets an obvious dead-store marker guarded by opaque control flow
			int x = rand() % ( 0x1337 + 1 );
			int y = 0x1337 - x;
			llvm::IRBuilder<llvm::NoFolder> no_fold_builder( body_clone_bb );
			llvm::Value* add = mba::make_add0( no_fold_builder, make_opaque_value( builder, builder.getInt32( x ), opaque_i32 ), make_opaque_value( builder, builder.getInt32( y ), opaque_i32 ) );
			builder.CreateStore( add, llvm::ConstantExpr::getIntToPtr( builder.getInt32( 0x1337 ), builder.getPtrTy() ) );

			// 3) Create an opaque jump from body clone -> tail/random path
			builder.CreateCondBr( make_pred(), tail_bb, random_block( { body_bb, body_clone_bb, tail_bb } ) );

			for ( llvm::PHINode& pn : tail_bb->phis() )
			{
				llvm::Value* incoming = pn.getIncomingValueForBlock( body_bb );
				if ( auto* incoming_i = llvm::dyn_cast_or_null<llvm::Instruction>( incoming ) )
				{
					if ( llvm::Value* mapped = v_map.lookup( incoming_i ) )
						incoming = mapped;
					pn.addIncoming( incoming, body_clone_bb );
				}
			}

			// Rewrite external users of values from original/clone through phis
			llvm::SmallVector<llvm::PHINode*, 8> new_ph_is;
			llvm::SSAUpdater updater( &new_ph_is );
			for ( llvm::Instruction& original_i : *body_bb )
			{
				llvm::Value* cloned_i = v_map.lookup( &original_i );

				bool has_outside_users = any_of( original_i.users(), [ & ]( llvm::User* u ) {
					if ( auto* i = llvm::dyn_cast<llvm::Instruction>( u ) )
					{
						llvm::BasicBlock* bb = i->getParent();
						return bb != body_bb && bb != body_clone_bb;
					}
					return false;
				} );
				if ( !has_outside_users )
					continue;

				updater.Initialize( original_i.getType(), original_i.getName() );
				updater.AddAvailableValue( body_bb, &original_i );
				updater.AddAvailableValue( body_clone_bb, cloned_i );

				for ( llvm::Use& u : make_early_inc_range( original_i.uses() ) )
				{
					auto* user_i = llvm::dyn_cast<llvm::Instruction>( u.getUser() );
					if ( !user_i )
						continue;
					llvm::BasicBlock* bb = user_i->getParent();
					if ( !llvm::isa<llvm::PHINode>( user_i ) && ( bb == body_bb || bb == body_clone_bb ) )
						continue;
					updater.RewriteUse( u );
				}
			}
		}

		return !worklist.empty();
	}
}  // namespace obf0::opq

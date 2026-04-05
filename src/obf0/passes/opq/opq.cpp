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
	static llvm::Value* make_opaque_value( llvm::IRBuilder<T>& Builder, llvm::Value* v )
	{
		llvm::AllocaInst* VolatileBool = Builder.CreateAlloca( v->getType() );
		Builder.CreateStore( v, VolatileBool, true );

		llvm::LoadInst* VolatileVal = Builder.CreateLoad( v->getType(), VolatileBool, true );
		return VolatileVal;
	}

	bool visit_opq( llvm::Function& F )
	{
		llvm::errs() << "(obf0-opq) obfuscating " << F.getName() << "\n";

		std::mt19937 random_generator( std::random_device{}() );
		llvm::IRBuilder<> Builder( &*F.getEntryBlock().getFirstInsertionPt() );

		// Pick a random block for the opaque "not target"
		auto random_block = [ & ]( std::vector<llvm::BasicBlock*> a ) {
			std::uniform_int_distribution<std::size_t> distribution( 0, a.size() - 1 );
			int idx = distribution( random_generator );
			return a[ idx ];
		};
		// Make a "very secure" opaque predicate
		auto make_pred = [ & ]() { return make_opaque_value( Builder, Builder.getInt1( true ) ); };

		std::vector<llvm::BasicBlock*> worklist;
		for ( llvm::BasicBlock& headBB : F )
		{
			// Skip no return, EH pad, or first block
			if ( llvm::isa<llvm::InvokeInst>( headBB.getTerminator() ) || headBB.isEHPad() || headBB.isEntryBlock() )
				continue;
			worklist.emplace_back( &headBB );
		}

		llvm::errs() << "(obf0-opq)\tobfuscating " << worklist.size() << " blocks\n";
		for ( llvm::BasicBlock* headBB : worklist )
		{
			// Split it into head -> body -> tail
			// Head: phi nodes, debug, etc.
			// Body: bulk of the block
			// Tail: last instruction
			llvm::BasicBlock* bodyBB = headBB->splitBasicBlock( headBB->getFirstNonPHIOrDbgOrLifetime() );
			llvm::BasicBlock* tailBB = bodyBB->splitBasicBlock( bodyBB->getTerminator() );

			// Clone body
			llvm::ValueToValueMapTy VMap;
			llvm::BasicBlock* bodyCloneBB = CloneBasicBlock( bodyBB, VMap, "", bodyBB->getParent() );
			llvm::SmallVector<llvm::BasicBlock*, 1> Blocks{ bodyCloneBB };
			remapInstructionsInBlocks( Blocks, VMap );

			// 1) Head and body used to be part of the same block, now they will be joined by an OP
			headBB->getTerminator()->eraseFromParent();
			// 2) Tail is the last instruction of body, thus body must be reconnected to tail by an OP
			bodyBB->getTerminator()->eraseFromParent();
			// 3) Body clone is a decoy which should also be attached to head
			bodyCloneBB->getTerminator()->eraseFromParent();

			// 1) Create an opaque jump from head -> body
			Builder.SetInsertPoint( headBB );
			Builder.CreateCondBr( make_pred(), bodyBB, bodyCloneBB );

			// 2) Create an opaque jump from body -> tail
			// Note: headBB can't be a target of the False condition
			Builder.SetInsertPoint( bodyBB );
			Builder.CreateCondBr( make_pred(), tailBB, random_block( { bodyBB, bodyCloneBB, tailBB } ) );

			Builder.SetInsertPoint( bodyCloneBB );
			// The cloned block gets an obvious dead-store marker guarded by opaque control flow
			int x = rand() % ( 0x1337 + 1 );
			int y = 0x1337 - x;
			llvm::IRBuilder<llvm::NoFolder> NoFoldBuilder( bodyCloneBB );
			llvm::Value* Add = mba::make_add0( NoFoldBuilder, make_opaque_value( Builder, Builder.getInt32( x ) ), make_opaque_value( Builder, Builder.getInt32( y ) ) );
			Builder.CreateStore( Add, llvm::ConstantExpr::getIntToPtr( Builder.getInt32( 0x1337 ), Builder.getPtrTy() ) );

			// 3) Create an opaque jump from body clone -> tail/random path
			Builder.CreateCondBr( make_pred(), tailBB, random_block( { bodyBB, bodyCloneBB, tailBB } ) );

			for ( llvm::PHINode& PN : tailBB->phis() )
			{
				llvm::Value* Incoming = PN.getIncomingValueForBlock( bodyBB );
				if ( auto* IncomingI = llvm::dyn_cast_or_null<llvm::Instruction>( Incoming ) )
				{
					if ( llvm::Value* Mapped = VMap.lookup( IncomingI ) )
						Incoming = Mapped;
					PN.addIncoming( Incoming, bodyCloneBB );
				}
			}

			// Rewrite external users of values from original/clone through phis
			llvm::SmallVector<llvm::PHINode*, 8> NewPHIs;
			llvm::SSAUpdater Updater( &NewPHIs );
			for ( llvm::Instruction& originalI : *bodyBB )
			{
				llvm::Value* clonedI = VMap.lookup( &originalI );

				bool HasOutsideUsers = any_of( originalI.users(), [ & ]( llvm::User* U ) {
					if ( auto* I = llvm::dyn_cast<llvm::Instruction>( U ) )
					{
						llvm::BasicBlock* BB = I->getParent();
						return BB != bodyBB && BB != bodyCloneBB;
					}
					return false;
				} );
				if ( !HasOutsideUsers )
					continue;

				Updater.Initialize( originalI.getType(), originalI.getName() );
				Updater.AddAvailableValue( bodyBB, &originalI );
				Updater.AddAvailableValue( bodyCloneBB, clonedI );

				for ( llvm::Use& U : make_early_inc_range( originalI.uses() ) )
				{
					auto* UserI = llvm::dyn_cast<llvm::Instruction>( U.getUser() );
					if ( !UserI )
						continue;
					llvm::BasicBlock* BB = UserI->getParent();
					if ( !llvm::isa<llvm::PHINode>( UserI ) && ( BB == bodyBB || BB == bodyCloneBB ) )
						continue;
					Updater.RewriteUse( U );
				}
			}
		}

		return !worklist.empty();
	}
}  // namespace obf0::opq

#include <obf0/passes/mba/mba.hpp>

#include <vector>

#include <llvm/IR/Constants.h>
#include <llvm/IR/InstrTypes.h>
#include <llvm/IR/NoFolder.h>
#include <llvm/Support/Casting.h>

#include <obf0/utility/llvm.hpp>

#define OPAQUE( x ) obf0::util::make_black_box( builder, x )
namespace obf0::mba
{
	llvm::Value* make_add0( llvm::IRBuilder<llvm::NoFolder>& builder, llvm::Value* lhs, llvm::Value* rhs )
	{
		// rewrite to (~A&B)+(A&~B)+((A&B)<<1)
		// llvm::Value* first = obf0::util::make_black_box( builder, builder.CreateAnd( builder.CreateNot( lhs ), rhs ) );
		llvm::Value* first = OPAQUE( builder.CreateAnd( builder.CreateNot( lhs ), rhs ) );

		llvm::Value* second = OPAQUE( builder.CreateAnd( lhs, builder.CreateNot( rhs ) ) );
		llvm::Value* third  = OPAQUE( builder.CreateShl( builder.CreateAnd( lhs, rhs ), llvm::ConstantInt::get( lhs->getType(), 1 ) ) );

		return builder.CreateAdd( builder.CreateAdd( first, second ), third );
	}

	llvm::Value* make_sub1( llvm::IRBuilder<llvm::NoFolder>& builder, llvm::Value* lhs, llvm::Value* rhs )
	{
		// rewrite to ~((~A)+B)
		llvm::Value* first = OPAQUE( builder.CreateAdd( builder.CreateNot( lhs ), rhs ) );
		return builder.CreateNot( first );
	}

	llvm::Value* make_sub0( llvm::IRBuilder<llvm::NoFolder>& builder, llvm::Value* x, llvm::Value* y )
	{
		using namespace llvm;

		// x ^ y
		Value* xor_xy = builder.CreateXor( x, y, "xor_xy" );

		// ~x  (bitwise NOT)
		Value* not_x = builder.CreateNot( x, "not_x" );

		// (~x & y)
		Value* and_term = builder.CreateAnd( not_x, y, "and_term" );

		// constant 2 (same type as x/y)
		Value* two = ConstantInt::get( x->getType(), 2 );

		// 2 * (~x & y)
		Value* mul_term = builder.CreateMul( two, and_term, "mul_term" );

		// (x ^ y) - 2 * (~x & y)
		Value* result = builder.CreateSub( xor_xy, mul_term, "mba_sub" );

		return result;
	}

	bool visit_mba( llvm::Function& f )
	{
		if ( f.isDeclaration() )
			return false;

		llvm::errs() << "(obf0-mba) obfuscating " << f.getName() << "\n";
		std::vector<llvm::BinaryOperator*> add_worklist;
		std::vector<llvm::Instruction*> sub_worklist;

		for ( auto& bb : f )
		{
			for ( auto& i : bb )
			{
				if ( llvm::BinaryOperator* bo = llvm::dyn_cast<llvm::BinaryOperator>( &i ) )
				{
					// MBA operates only on integer types
					if ( !bo->getType()->isIntegerTy() )
						continue;

					// don't modify the basic block while iterating the basic block
					switch ( bo->getOpcode() )
					{
						case llvm::Instruction::Add: {
							llvm::errs() << "(obf0-mba)\tqueuing rewrite (add) " << i << "\n";
							add_worklist.emplace_back( bo );
							break;
						}

						case llvm::Instruction::Sub: {
							llvm::errs() << "(obf0-mba)\tqueuing rewrite (sub) " << i << "\n";
							sub_worklist.emplace_back( bo );
							break;
						}

						default:
							break;
					}
				}
			}
		}

		// We need the block context to avoid badref, so we need to make it a child of BO temporarily
		// Then we can replace the original instruction
		for ( auto from : add_worklist )
		{
			llvm::IRBuilder<llvm::NoFolder> builder( from );
			llvm::Value* to = make_add0( builder, from->getOperand( 0 ), from->getOperand( 1 ) );
			from->replaceAllUsesWith( to );
			from->eraseFromParent();
		}

		for ( auto from : sub_worklist )
		{
			llvm::IRBuilder<llvm::NoFolder> builder( from );
			llvm::Value* to = make_sub0( builder, from->getOperand( 0 ), from->getOperand( 1 ) );
			from->replaceAllUsesWith( to );
			from->eraseFromParent();
		}

		return !( add_worklist.empty() && sub_worklist.empty() );
	}
}  // namespace obf0::mba

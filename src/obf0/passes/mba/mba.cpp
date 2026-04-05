#include <obf0/passes/mba/mba.hpp>

#include <vector>

#include <llvm/IR/Constants.h>
#include <llvm/IR/InstrTypes.h>
#include <llvm/Support/Casting.h>

namespace obf0::mba
{
	llvm::Value* make_add0( llvm::IRBuilder<llvm::NoFolder>& Builder, llvm::Value* lhs, llvm::Value* rhs )
	{
		// rewrite to (~A&B)+(A&~B)+((A&B)<<1)
		llvm::Value* first  = Builder.CreateAnd( Builder.CreateNot( lhs ), rhs );
		llvm::Value* second = Builder.CreateAnd( lhs, Builder.CreateNot( rhs ) );
		llvm::Value* third  = Builder.CreateShl( Builder.CreateAnd( lhs, rhs ), llvm::ConstantInt::get( lhs->getType(), 1 ) );

		return Builder.CreateAdd( Builder.CreateAdd( first, second ), third );
	}

	llvm::Value* make_sub0( llvm::IRBuilder<llvm::NoFolder>& Builder, llvm::Value* lhs, llvm::Value* rhs )
	{
		// rewrite to ~((~A)+B)
		llvm::Value* first = Builder.CreateAdd( Builder.CreateNot( lhs ), rhs );
		return Builder.CreateNot( first );
	}

	bool visit_mba( llvm::Function& F )
	{
		llvm::errs() << "(obf0-mba) obfuscating " << F.getName() << "\n";
		std::vector<llvm::BinaryOperator*> add_worklist;
		std::vector<llvm::Instruction*> sub_worklist;

		for ( auto& BB : F )
		{
			for ( auto& I : BB )
			{
				if ( llvm::BinaryOperator* BO = llvm::dyn_cast<llvm::BinaryOperator>( &I ) )
				{
					// MBA operates only on integer types
					if ( !BO->getType()->isIntegerTy() )
						continue;

					// don't modify the basic block while iterating the basic block
					switch ( BO->getOpcode() )
					{
						case llvm::Instruction::Add: {
							llvm::errs() << "(obf0-mba)\tqueuing rewrite (add) " << I << "\n";
							add_worklist.emplace_back( BO );
							break;
						}

						case llvm::Instruction::Sub: {
							llvm::errs() << "(obf0-mba)\tqueuing rewrite (sub) " << I << "\n";
							sub_worklist.emplace_back( BO );
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
			llvm::IRBuilder<llvm::NoFolder> Builder( from );
			llvm::Value* to = make_add0( Builder, from->getOperand( 0 ), from->getOperand( 1 ) );
			from->replaceAllUsesWith( to );
			from->eraseFromParent();
		}

		for ( auto from : sub_worklist )
		{
			llvm::IRBuilder<llvm::NoFolder> Builder( from );
			llvm::Value* to = make_sub0( Builder, from->getOperand( 0 ), from->getOperand( 1 ) );
			from->replaceAllUsesWith( to );
			from->eraseFromParent();
		}

		return !( add_worklist.empty() && sub_worklist.empty() );
	}
}  // namespace obf0::mba

#include <tuple>
#include <vector>

#include <llvm/IR/Constant.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/InstrTypes.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/NoFolder.h>
#include <llvm/Support/Casting.h>

namespace obf0::icall
{
	bool visit_icall( llvm::Function& F )
	{
		llvm::errs() << "(obf0-icall) obfuscating " << F.getName() << "\n";
		std::vector<std::tuple<llvm::CallInst*>> worklist;

		for ( auto& BB : F )
		{
			for ( auto& I : BB )
			{
				auto* CI = llvm::dyn_cast<llvm::CallInst>( &I );
				if ( !CI )
					continue;

				auto* Callee = CI->getCalledFunction();

				if ( !Callee || Callee->isIntrinsic() )
					continue;

				worklist.push_back( std::make_tuple( CI ) );
			}
		}

		llvm::IRBuilder<> Builder( F.getContext() );

		auto* IntPtrTy = F.getParent()->getDataLayout().getIntPtrType( F.getContext() );
		auto Magic     = 0x1337;

		for ( auto work : worklist )
		{
			auto* CI = std::get<0>( work );
			Builder.SetInsertPoint( CI );

			// Oh, if only this was folded at compile time!
			auto* A = llvm::ConstantExpr::getPtrToInt( CI->getCalledFunction(), IntPtrTy );
			auto* B = llvm::ConstantExpr::getAdd( A, llvm::ConstantInt::get( IntPtrTy, Magic ) );

			auto* VolatileAddr = Builder.CreateAlloca( IntPtrTy );
			Builder.CreateStore( B, VolatileAddr, true );
			auto* VolatileVal = Builder.CreateLoad( IntPtrTy, VolatileAddr, true );

			auto* C = Builder.CreateSub( VolatileVal, llvm::ConstantInt::get( IntPtrTy, Magic ) );

			CI->setCalledOperand( Builder.CreateIntToPtr( C, Builder.getPtrTy() ) );
		}

		return !( worklist.empty() );
	}
}  // namespace obf0::icall

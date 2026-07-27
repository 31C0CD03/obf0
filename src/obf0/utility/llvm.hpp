#ifndef OBF0_UTIL_H
#define OBF0_UTIL_H

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/InlineAsm.h>
#include <llvm/Support/raw_ostream.h>

namespace obf0::util
{
	template<typename T>
	llvm::Value* make_black_box( llvm::IRBuilder<T>& builder, llvm::Value* input )
	{
		auto ty = input->getType();
		if ( !ty->isSingleValueType() )
		{
			llvm::errs() << "warn! not single value type\n";
			return input;
		}
		auto fty      = llvm::FunctionType::get( ty, { ty }, false );
		auto blackbox = llvm::InlineAsm::get( fty, "", "=r,0,~{memory}", true );
		return builder.CreateCall( blackbox, { input } );
	}
}

#endif  // OBF0_UTIL_H

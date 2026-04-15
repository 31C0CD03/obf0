#ifndef OBF0_SRC_ICALL
#define OBF0_SRC_ICALL

#include <type_traits>

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/NoFolder.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/raw_ostream.h>

#include <obf0/secpol.hpp>

namespace obf0::icall
{
	bool visit_icall( llvm::Function& F );

	template<typename T>
	struct IndirectCall : llvm::PassInfoMixin<IndirectCall<T>>
	{
		static_assert( std::is_base_of<secpol::policy, T>::value );

		T m_policy;

		IndirectCall() : m_policy( T{} ) {}
		IndirectCall( T policy ) : m_policy( policy ) {}

		llvm::PreservedAnalyses run( llvm::Function& F, llvm::FunctionAnalysisManager& )
		{
			if ( !m_policy.is_applicable( F.getName().str() ) )
				return llvm::PreservedAnalyses::all();

			bool changed = false;
			changed |= visit_icall( F );

			if ( !verifyFunction( F, &llvm::errs() ) )
				llvm::errs() << "verified\n";
			return changed ? llvm::PreservedAnalyses::none() : llvm::PreservedAnalyses::all();
		}

		// don't skip optnone
		static bool isRequired() { return true; }
	};
}  // namespace obf0::icall

#endif  // OBF0_SRC_ICALL

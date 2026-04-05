#ifndef OBF0_SRC_MBA
#define OBF0_SRC_MBA

#include <type_traits>

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/NoFolder.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/raw_ostream.h>

#include <obf0/secpol.hpp>

namespace obf0::mba
{
	llvm::Value* make_add0( llvm::IRBuilder<llvm::NoFolder>& Builder, llvm::Value* lhs, llvm::Value* rhs );
	llvm::Value* make_sub0( llvm::IRBuilder<llvm::NoFolder>& Builder, llvm::Value* lhs, llvm::Value* rhs );
	bool visit_mba( llvm::Function& F );

	template<typename T>
	struct MixedBooleanArith : llvm::PassInfoMixin<MixedBooleanArith<T>>
	{
		static_assert( std::is_base_of<secpol::policy, T>::value );

		int m_depth;
		T m_policy;

		MixedBooleanArith( int depth = 3 ) : m_depth( depth ), m_policy( T{} ) {}
		MixedBooleanArith( int depth, T policy ) : m_depth( depth ), m_policy( policy ) {}

		llvm::PreservedAnalyses run( llvm::Function& F, llvm::FunctionAnalysisManager& )
		{
			if ( !m_policy.is_applicable( F.getName().str() ) )
				return llvm::PreservedAnalyses::all();

			bool changed = false;
			for ( int i = 0; i < this->m_depth; i++ )
				changed |= visit_mba( F );
			if ( !verifyFunction( F, &llvm::errs() ) )
				llvm::errs() << "verified\n";
			return changed ? llvm::PreservedAnalyses::none() : llvm::PreservedAnalyses::all();
		}

		// don't skip optnone
		static bool isRequired() { return true; }
	};
}  // namespace obf0::mba

#endif  // OBF0_SRC_MBA

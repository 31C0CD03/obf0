#ifndef OBF0_SRC_OPQ
#define OBF0_SRC_OPQ

#include <type_traits>

#include <llvm/IR/Function.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/raw_ostream.h>

#include <obf0/secpol.hpp>

namespace obf0::opq
{
	bool visit_opq( llvm::Function& f );

	template<typename T>
	struct opaque_predicates : llvm::PassInfoMixin<opaque_predicates<T>>
	{
		static_assert( std::is_base_of<secpol::policy, T>::value );

		int m_depth;
		T m_policy;

		opaque_predicates( int depth = 1 ) : m_depth( depth ), m_policy( T{} ) {}
		opaque_predicates( int depth, T policy ) : m_depth( depth ), m_policy( policy ) {}

		llvm::PreservedAnalyses run( llvm::Function& f, llvm::FunctionAnalysisManager& )
		{
			if ( !m_policy.is_applicable( f.getName().str() ) )
				return llvm::PreservedAnalyses::all();

			bool changed = false;
			for ( int i = 0; i < m_depth; i++ )
				changed |= visit_opq( f );
			if ( !verifyFunction( f, &llvm::errs() ) )
				llvm::errs() << "verified\n";
			return changed ? llvm::PreservedAnalyses::none() : llvm::PreservedAnalyses::all();
		}

		// don't skip optnone
		static bool isRequired() { return true; }  // NOLINT(readability-identifier-naming)
	};
}  // namespace obf0::opq

#endif  // OBF0_SRC_OPQ

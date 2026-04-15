#include <obf0/passes/icall/icall.hpp>
#include <obf0/passes/mba/mba.hpp>
#include <obf0/passes/opq/opq.hpp>
#include <obf0/secpol.hpp>

#include <llvm/Config/llvm-config.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Passes/PassPlugin.h>

namespace
{
	static const char* PLUGIN_NAME = "obf0";

	static const auto MBA_DEPTH  = 2;
	static const auto MBA_POLICY = obf0::secpol::uniform_policy( false );

	static const auto OPQ_DEPTH  = 2;
	static const auto OPQ_POLICY = obf0::secpol::uniform_policy( false );

	static const auto ICALL_POLICY = obf0::secpol::uniform_policy();

	void addObf0Passes( llvm::FunctionPassManager& FPM )
	{
		FPM.addPass( obf0::mba::MixedBooleanArith<decltype( MBA_POLICY )>( MBA_DEPTH, MBA_POLICY ) );
		FPM.addPass( obf0::opq::OpaquePredicates<decltype( OPQ_POLICY )>( OPQ_DEPTH, OPQ_POLICY ) );
		FPM.addPass( obf0::icall::IndirectCall<decltype( ICALL_POLICY )>( ICALL_POLICY ) );
	}

	llvm::PassPluginLibraryInfo getobf0PluginInfo()
	{
		return { LLVM_PLUGIN_API_VERSION, PLUGIN_NAME, LLVM_VERSION_STRING, []( llvm::PassBuilder& PB ) {
					PB.registerOptimizerLastEPCallback( []( llvm::ModulePassManager& MPM, llvm::OptimizationLevel, llvm::ThinOrFullLTOPhase ) {
						llvm::FunctionPassManager FPM;
						addObf0Passes( FPM );
						MPM.addPass( llvm::createModuleToFunctionPassAdaptor( std::move( FPM ) ) );
					} );
					PB.registerPipelineParsingCallback( []( llvm::StringRef PassName, llvm::FunctionPassManager& FPM, llvm::ArrayRef<llvm::PassBuilder::PipelineElement> ) {
						if ( PassName != PLUGIN_NAME )
							return false;

						addObf0Passes( FPM );
						return true;
					} );

					PB.registerPipelineParsingCallback( []( llvm::StringRef PassName, llvm::ModulePassManager& MPM, llvm::ArrayRef<llvm::PassBuilder::PipelineElement> ) {
						if ( PassName != PLUGIN_NAME )
							return false;

						llvm::FunctionPassManager FPM;
						addObf0Passes( FPM );
						MPM.addPass( llvm::createModuleToFunctionPassAdaptor( std::move( FPM ) ) );
						return true;
					} );
				} };
	}
}  // namespace

extern "C" LLVM_ATTRIBUTE_WEAK ::llvm::PassPluginLibraryInfo llvmGetPassPluginInfo() { return getobf0PluginInfo(); }

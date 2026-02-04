#include <cstddef>
#include <llvm/CodeGen/MachineFunction.h>
#include <llvm/IR/Constants.h>
#include <random>
#include <type_traits>
#include <vector>

#include <llvm/Pass.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Passes/PassPlugin.h>

#include <llvm/IR/Analysis.h>
#include <llvm/IR/Attributes.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/CFG.h>
#include <llvm/IR/Constant.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/InstrTypes.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/Use.h>
#include <llvm/IR/User.h>

#include <llvm/Transforms/Utils/BasicBlockUtils.h>
#include <llvm/Transforms/Utils/Cloning.h>
#include <llvm/Transforms/Utils/SSAUpdater.h>

#include <llvm/Config/llvm-config.h>
#include <llvm/Support/Casting.h>

#include <llvm/IR/NoFolder.h>
#include <llvm/IR/Verifier.h>

#include "secpol.h"
#include <vector>

namespace obf0 {

llvm::Value *make_add0(llvm::IRBuilder<> &Builder, llvm::Value *lhs, llvm::Value *rhs) {
    // rewrite to (~A&B)+(A&~B)+((A&B)<<1)
    llvm::Value *first = Builder.CreateAnd(Builder.CreateNot(lhs), rhs);
    llvm::Value *second = Builder.CreateAnd(lhs, Builder.CreateNot(rhs));
    llvm::Value *third = Builder.CreateShl(Builder.CreateAnd(lhs, rhs), llvm::ConstantInt::get(lhs->getType(), 1));

    return Builder.CreateAdd(Builder.CreateAdd(first, second), third);
}

llvm::Value *make_sub0(llvm::IRBuilder<> &Builder, llvm::Value *lhs, llvm::Value *rhs) {
    // rewrite to ~((~A)+B)
    llvm::Value *first = Builder.CreateAdd(Builder.CreateNot(lhs), rhs);
    return Builder.CreateNot(first);
}

llvm::Value *make_opaque_value(llvm::IRBuilder<> &Builder, llvm::Value *v) {
    llvm::AllocaInst *VolatileBool = Builder.CreateAlloca(v->getType());
    Builder.CreateStore(v, VolatileBool, true);

    llvm::LoadInst *VolatileVal = Builder.CreateLoad(v->getType(), VolatileBool, true);
    return VolatileVal;
}

bool visit_opq(llvm::Function &F) {
    llvm::errs() << "(obf0-opq) obfuscating " << F.getName() << "\n";

    std::mt19937 random_generator(std::random_device{}());
    llvm::IRBuilder<> Builder(&*F.getEntryBlock().getFirstInsertionPt());

    // Pick a random block for the opaque "not target"
    auto random_block = [&](std::vector<llvm::BasicBlock *> a) {
        std::uniform_int_distribution<std::size_t> distribution(0, a.size() - 1);
        int idx = distribution(random_generator);
        return a[idx];
    };
    // Make a "very secure" opaque predicate
    auto make_pred = [&]() { return make_opaque_value(Builder, Builder.getInt1(true)); };

    std::vector<llvm::BasicBlock *> worklist;
    for (llvm::BasicBlock &headBB : F) {
        // Skip no return, EH pad, or first block
        if (llvm::isa<llvm::InvokeInst>(headBB.getTerminator()) || headBB.isEHPad() || headBB.isEntryBlock())
            continue;
        worklist.emplace_back(&headBB);
    }

    llvm::errs() << "(obf0-opq)\tobfuscating " << worklist.size() << " blocks\n";
    for (llvm::BasicBlock *headBB : worklist) {
        // Split it into head -> body -> tail
        // Head: phi nodes, debug, etc.
        // Body: bulk of the block
        // Tail: last instruction
        llvm::BasicBlock *bodyBB = headBB->splitBasicBlock(headBB->getFirstNonPHIOrDbgOrLifetime());
        llvm::BasicBlock *tailBB = bodyBB->splitBasicBlock(bodyBB->getTerminator());

        // Clone body
        llvm::ValueToValueMapTy VMap;
        llvm::BasicBlock *bodyCloneBB = CloneBasicBlock(bodyBB, VMap, "", bodyBB->getParent());
        // remapInstructionsInBlock(bodyBB, bodyCloneBB, VMap);
        llvm::SmallVector<llvm::BasicBlock *, 1> Blocks{bodyCloneBB};
        remapInstructionsInBlocks(Blocks, VMap);

        int oldPred = pred_size(tailBB);

        // 1) Head and body used to be part of the same block, now they will be joined by an OP
        headBB->getTerminator()->eraseFromParent();
        // 2) Tail is the last instruction of body, thus body must be reconnected to tail by an OP
        bodyBB->getTerminator()->eraseFromParent();
        // 3) Body clone is a decoy which should also be attached to head
        bodyCloneBB->getTerminator()->eraseFromParent();

        // 1) Create an opaque jump from head -> body
        Builder.SetInsertPoint(headBB);
        // Builder.CreateBr(bodyBB);
        Builder.CreateCondBr(make_pred(), bodyBB, bodyCloneBB);

        // 2) Create an opaque jump from body -> tail
        // Note: headBB can't be a target of the False condition, this is obviously because LLVM hates me and will do
        // everything in its power to prevent me making a cool CFG
        Builder.SetInsertPoint(bodyBB);
        // Builder.CreateBr(tailBB);
        Builder.CreateCondBr(make_pred(), tailBB, random_block({bodyBB, bodyCloneBB, tailBB}));

        Builder.SetInsertPoint(bodyCloneBB);
        // Identifying the clone block isn't easy when debugging the IR graph / raw disassembly so inject a fun little
        // *((int*)0x1337) = 0x1337; with a bunch of MBA to make it obvious that this code wouldn't be executed anyway
        int x = rand() % (0x1337 + 1);
        int y = 0x1337 - x;
        llvm::Value *Add = make_add0(Builder, make_opaque_value(Builder, Builder.getInt32(x)),
                                     make_opaque_value(Builder, Builder.getInt32(y)));
        Builder.CreateStore(Add, llvm::ConstantExpr::getIntToPtr(Builder.getInt32(0x1337), Builder.getPtrTy()));
        // 3) Create an opaque jump from body clone -> body
        // See above note on opaque targets
        // Builder.CreateBr(tailBB);
        Builder.CreateCondBr(make_pred(), tailBB, random_block({bodyBB, bodyCloneBB, tailBB}));

        for (llvm::PHINode &PN : tailBB->phis()) {
            // if this phi is referencing the original block and is an instruction
            llvm::Value *Incoming = PN.getIncomingValueForBlock(bodyBB);
            if (auto *IncomingI = llvm::dyn_cast_or_null<llvm::Instruction>(Incoming)) {
                // remap it if required
                if (llvm::Value *Mapped = VMap.lookup(IncomingI))
                    Incoming = Mapped;
                // adjust phi
                PN.addIncoming(Incoming, bodyCloneBB);
            }
        }

        // rewrite all future references of our values defined in the original block / cloned block with phis
        llvm::SmallVector<llvm::PHINode *, 8> NewPHIs;
        llvm::SSAUpdater Updater(&NewPHIs);
        for (llvm::Instruction &originalI : *bodyBB) {
            llvm::Value *clonedI = VMap.lookup(&originalI);

            bool HasOutsideUsers = any_of(originalI.users(), [&](llvm::User *U) {
                if (auto *I = llvm::dyn_cast<llvm::Instruction>(U)) {
                    llvm::BasicBlock *BB = I->getParent();
                    return BB != bodyBB && BB != bodyCloneBB;
                }
                return false;
            });
            if (!HasOutsideUsers)
                continue;

            // %new = phi i64 [ %originalI, %originalBB ], [ %clonedI, %clonedBB ]
            Updater.Initialize(originalI.getType(), originalI.getName());
            Updater.AddAvailableValue(bodyBB, &originalI);
            Updater.AddAvailableValue(bodyCloneBB, clonedI);

            // ReplaceAllUsesOf %originalI WITH %new WHERE (NOT phi) AND (NOT IN originalBB) AND (NOT IN clonedBB)
            for (llvm::Use &U : make_early_inc_range(originalI.uses())) {
                auto *UserI = llvm::dyn_cast<llvm::Instruction>(U.getUser());
                if (!UserI)
                    continue;
                llvm::BasicBlock *BB = UserI->getParent();
                if (!llvm::isa<llvm::PHINode>(UserI) && (BB == bodyBB || BB == bodyCloneBB))
                    continue;
                Updater.RewriteUse(U);
            }
        }
    }
    return !worklist.empty();
}

bool visit_mba(llvm::Function &F) {
    llvm::errs() << "(obf0-mba) obfuscating " << F.getName() << "\n";
    std::vector<llvm::BinaryOperator *> add_worklist;
    std::vector<llvm::Instruction *> sub_worklist;

    for (auto &BB : F) {
        for (auto &I : BB) {
            if (llvm::BinaryOperator *BO = llvm::dyn_cast<llvm::BinaryOperator>(&I)) {
                // MBA operates only on integer types
                if (!BO->getType()->isIntegerTy())
                    continue;

                // don't modify the basic block while iterating the basic block
                switch (BO->getOpcode()) {
                case llvm::Instruction::Add: {
                    llvm::errs() << "(obf0-mba)\tqueuing rewrite (add) " << I << "\n";
                    add_worklist.emplace_back(BO);
                    break;
                }

                case llvm::Instruction::Sub: {
                    llvm::errs() << "(obf0-mba)\tqueuing rewrite (sub) " << I << "\n";
                    sub_worklist.emplace_back(BO);
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
    for (auto from : add_worklist) {
        // break;
        llvm::IRBuilder<> Builder(from);
        llvm::Value *to = make_add0(Builder, from->getOperand(0), from->getOperand(1));
        // dodge constant folding ?
        from->replaceAllUsesWith(to);
        from->eraseFromParent();
    }

    for (auto from : sub_worklist) {
        llvm::IRBuilder<> Builder(from);
        llvm::Value *to = make_sub0(Builder, from->getOperand(0), from->getOperand(1));
        // dodge constant folding ?
        from->replaceAllUsesWith(to);
        from->eraseFromParent();
    }

    return !(add_worklist.empty() && sub_worklist.empty());
}

template <typename T> struct MixedBooleanArith : llvm::PassInfoMixin<MixedBooleanArith<T>> {
    static_assert(std::is_base_of<secpol::policy, T>::value);

    int m_depth;
    T m_policy;

    MixedBooleanArith(int depth = 3) : m_depth(depth), m_policy(T{}) {}
    MixedBooleanArith(int depth, T policy) : m_depth(depth), m_policy(policy) {}

    llvm::PreservedAnalyses run(llvm::Function &F, llvm::FunctionAnalysisManager &) {
        if (!m_policy.is_applicable(F.getName().str()))
            return llvm::PreservedAnalyses::all();

        bool changed = false;
        for (int i = 0; i < this->m_depth; i++)
            changed |= visit_mba(F);
        if (!verifyFunction(F, &llvm::errs()))
            llvm::errs() << "verified\n";
        return changed ? llvm::PreservedAnalyses::none() : llvm::PreservedAnalyses::all();
    }

    // don't skip optnone
    static bool isRequired() { return true; }
};

template <typename T> struct OpaquePredicates : llvm::PassInfoMixin<OpaquePredicates<T>> {
    static_assert(std::is_base_of<secpol::policy, T>::value);

    int m_depth;
    T m_policy;

    OpaquePredicates(int depth = 1) : m_depth(depth), m_policy(T{}) {}
    OpaquePredicates(int depth, T policy) : m_depth(depth), m_policy(policy) {}

    llvm::PreservedAnalyses run(llvm::Function &F, llvm::FunctionAnalysisManager &) {
        if (!m_policy.is_applicable(F.getName().str()))
            return llvm::PreservedAnalyses::all();

        bool changed = false;
        for (int i = 0; i < m_depth; i++)
            changed |= visit_opq(F);
        if (!verifyFunction(F, &llvm::errs()))
            llvm::errs() << "verified\n";
        return changed ? llvm::PreservedAnalyses::none() : llvm::PreservedAnalyses::all();
    }

    // don't skip optnone
    static bool isRequired() { return true; }
};
} // namespace obf0

llvm::PassPluginLibraryInfo getobf0PluginInfo() {
    return {LLVM_PLUGIN_API_VERSION, "obf0", LLVM_VERSION_STRING, [](llvm::PassBuilder &PB) {
                PB.registerOptimizerLastEPCallback([&](llvm::ModulePassManager &MPM, auto, auto) { return true; });
                PB.registerPipelineParsingCallback([](llvm::StringRef PassName, llvm::FunctionPassManager &FPM, ...) {
                    FPM.addPass(obf0::MixedBooleanArith<obf0::secpol::allow_policy>(6, obf0::secpol::allow_policy({"foo"})));
                    FPM.addPass(obf0::OpaquePredicates<obf0::secpol::allow_policy>(1, obf0::secpol::allow_policy({"foo"})));
                    return true;
                });
            }};
}

extern "C" LLVM_ATTRIBUTE_WEAK ::llvm::PassPluginLibraryInfo llvmGetPassPluginInfo() { return getobf0PluginInfo(); }

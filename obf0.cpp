#include <cstddef>
#include <llvm/CodeGen/MachineFunction.h>
#include <llvm/IR/Constants.h>
#include <random>
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

#include <llvm/IR/Verifier.h>
#include <llvm/IR/NoFolder.h>

using namespace llvm;

namespace {

template <typename T>
Value *make_add0(IRBuilder<T> &Builder, Value *lhs, Value *rhs) {
    // rewrite to (~A&B)+(A&~B)+((A&B)<<1)
    Value *first = Builder.CreateAnd(Builder.CreateNot(lhs), rhs);
    Value *second = Builder.CreateAnd(lhs, Builder.CreateNot(rhs));
    Value *third = Builder.CreateShl(Builder.CreateAnd(lhs, rhs), ConstantInt::get(lhs->getType(), 1));

    return Builder.CreateAdd(Builder.CreateAdd(first, second), third);
}

Value *make_sub0(IRBuilder<> &Builder, Value *lhs, Value *rhs) {
    // rewrite to ~((~A)+B)
    Value *first = Builder.CreateAdd(Builder.CreateNot(lhs), rhs);
    return Builder.CreateNot(first);
}

// BasicBlock *remapInstructionsInBlock(BasicBlock *originalBB, BasicBlock *clonedBB, ValueToValueMapTy &VMap) {
//     BasicBlock::iterator originalInst = originalBB->begin();
//     for (Instruction &clonedInst : *clonedBB) {
//         // remap operands
//         for (unsigned i = 0; i < clonedInst.getNumOperands(); i++) {
//             Value *V = MapValue(clonedInst.getOperand(i), VMap);
//             if (V) {
//                 clonedInst.setOperand(i, V);
//             }
//         }

//         // remap phi nodes incoming blocks.
//         if (PHINode *pn = dyn_cast<PHINode>(originalInst)) {
//             for (unsigned j = 0, e = pn->getNumIncomingValues(); j != e; ++j) {
//                 Value *v = MapValue(pn->getIncomingBlock(j), VMap, RF_None, 0);
//                 if (v != 0) {
//                     pn->setIncomingBlock(j, cast<BasicBlock>(v));
//                 }
//             }
//         }

//         // remap metadata
//         SmallVector<std::pair<unsigned, MDNode *>, 4> MDs;
//         clonedInst.getAllMetadata(MDs);
//         for (std::pair<unsigned, MDNode *> pair : MDs) {
//             MDNode *MD = MapMetadata(pair.second, VMap);
//             if (MD) {
//                 clonedInst.setMetadata(pair.first, MD);
//             }
//         }
//         // copy debug info (dwarf -g)
//         clonedInst.setDebugLoc(originalInst->getDebugLoc());
//         originalInst++;
//     }

//     // remap existing phi nodes in successors
//     for (BasicBlock *Succ : successors(clonedBB)) {
//         errs() << "(obf0-opq) fixing up clone successors\n";
//         // if there are phis in this successor
//         if (!Succ->phis().empty()) {
//             errs() << "(obf0-opq) fixing up clone successor\n";
//             // for each phi
//             for (PHINode &PN : Succ->phis()) {
//                 // if this phi is referencing the original block and is an instruction
//                 Value *Incoming = PN.getIncomingValueForBlock(originalBB);
//                 if (auto *IncomingI = dyn_cast_or_null<Instruction>(Incoming)) {
//                     // remap it if required
//                     if (Value *Mapped = VMap.lookup(IncomingI)) Incoming = Mapped;
//                     // adjust phi
//                     PN.addIncoming(Incoming, clonedBB);
//                 }
//             }
//         }
//     }
//     return clonedBB;
// }

Value *make_opaque_value(IRBuilder<> &Builder, Value *v) {
    AllocaInst *VolatileBool = Builder.CreateAlloca(v->getType());
    Builder.CreateStore(v, VolatileBool, true);

    LoadInst *VolatileVal = Builder.CreateLoad(v->getType(), VolatileBool, true);
    return VolatileVal;
}

bool visit_opq(Function &F) {
    errs() << "(obf0-opq) obfuscating " << F.getName() << "\n";

    std::mt19937 random_generator(std::random_device{}());
    IRBuilder<> Builder(&*F.getEntryBlock().getFirstInsertionPt());

    // Pick a random block for the opaque "not target"
    auto random_block = [&](std::vector<BasicBlock *> a) {
        std::uniform_int_distribution<std::size_t> distribution(0, a.size() - 1);
        int idx = distribution(random_generator);
        return a[idx];
    };
    // Make a "very secure" opaque predicate
    auto make_pred = [&]() { return make_opaque_value(Builder, Builder.getInt1(true)); };

    std::vector<BasicBlock *> worklist;
    for (BasicBlock &headBB : F) {
        // Skip no return, EH pad, or first block
        if (isa<InvokeInst>(headBB.getTerminator()) || headBB.isEHPad() || headBB.isEntryBlock())
            continue;
        worklist.emplace_back(&headBB);
    }

    errs() << "(obf0-opq)\tobfuscating " << worklist.size() << " blocks\n";
    for (BasicBlock *headBB : worklist) {
        // Split it into head -> body -> tail
        // Head: phi nodes, debug, etc.
        // Body: bulk of the block
        // Tail: last instruction
        BasicBlock *bodyBB = headBB->splitBasicBlock(headBB->getFirstNonPHIOrDbgOrLifetime());
        BasicBlock *tailBB = bodyBB->splitBasicBlock(bodyBB->getTerminator());

        // Clone body
        ValueToValueMapTy VMap;
        BasicBlock *bodyCloneBB = CloneBasicBlock(bodyBB, VMap, "", bodyBB->getParent());
        // remapInstructionsInBlock(bodyBB, bodyCloneBB, VMap);
        SmallVector<BasicBlock *, 1> Blocks{bodyCloneBB};
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
        Value *Add = make_add0(Builder, make_opaque_value(Builder, Builder.getInt32(x)),
                               make_opaque_value(Builder, Builder.getInt32(y)));
        Builder.CreateStore(Add, ConstantExpr::getIntToPtr(Builder.getInt32(0x1337), Builder.getPtrTy()));
        // 3) Create an opaque jump from body clone -> body
        // See above note on opaque targets
        // Builder.CreateBr(tailBB);
        Builder.CreateCondBr(make_pred(), tailBB, random_block({bodyBB, bodyCloneBB, tailBB}));

        for (PHINode &PN : tailBB->phis()) {
            // if this phi is referencing the original block and is an instruction
            Value *Incoming = PN.getIncomingValueForBlock(bodyBB);
            if (auto *IncomingI = dyn_cast_or_null<Instruction>(Incoming)) {
                // remap it if required
                if (Value *Mapped = VMap.lookup(IncomingI))
                    Incoming = Mapped;
                // adjust phi
                PN.addIncoming(Incoming, bodyCloneBB);
            }
        }

        // rewrite all future references of our values defined in the original block / cloned block with phis
        SmallVector<PHINode *, 8> NewPHIs;
        SSAUpdater Updater(&NewPHIs);
        for (Instruction &originalI : *bodyBB) {
            Value *clonedI = VMap.lookup(&originalI);

            bool HasOutsideUsers = any_of(originalI.users(), [&](User *U) {
                if (auto *I = dyn_cast<Instruction>(U)) {
                    BasicBlock *BB = I->getParent();
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
            for (Use &U : make_early_inc_range(originalI.uses())) {
                auto *UserI = dyn_cast<Instruction>(U.getUser());
                if (!UserI)
                    continue;
                BasicBlock *BB = UserI->getParent();
                if (!isa<PHINode>(UserI) && (BB == bodyBB || BB == bodyCloneBB))
                    continue;
                Updater.RewriteUse(U);
            }
        }
    }
    return !worklist.empty();
}

bool visit_mba(Function &F) {
    errs() << "(obf0-mba) obfuscating " << F.getName() << "\n";
    // if (true) return;
    std::vector<BinaryOperator *> add_worklist;
    std::vector<Instruction *> sub_worklist;

    for (auto &BB : F) {
        for (auto &I : BB) {
            if (BinaryOperator *BO = dyn_cast<BinaryOperator>(&I)) {
                if (!BO->getType()->isIntegerTy())
                    continue;

                // don't modify the basic block while iterating the basic block
                switch (BO->getOpcode()) {
                case llvm::Instruction::Add: {
                    errs() << "(obf0-mba)\tqueuing rewrite (add) " << I << "\n";
                    add_worklist.emplace_back(BO);
                    break;
                }

                case llvm::Instruction::Sub: {
                    errs() << "(obf0-mba)\tqueuing rewrite (sub) " << I << "\n";
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
        IRBuilder<> Builder(from);
        Value *to = make_add0(Builder, from->getOperand(0), from->getOperand(1));
        // dodge constant folding ?
        from->replaceAllUsesWith(to);
        from->eraseFromParent();
    }

    for (auto from : sub_worklist) {
        IRBuilder<> Builder(from);
        Value *to = make_sub0(Builder, from->getOperand(0), from->getOperand(1));
        // dodge constant folding ?
        from->replaceAllUsesWith(to);
        from->eraseFromParent();
    }

    return !(add_worklist.empty() && sub_worklist.empty());
}

struct MixedBooleanArith : PassInfoMixin<MixedBooleanArith> {
    int m_depth;
    MixedBooleanArith(int depth = 3) : m_depth(depth) {}

    PreservedAnalyses run(Function &F, FunctionAnalysisManager &) {
        bool changed = false;
        for (int i = 0; i < this->m_depth; i++) changed |= visit_mba(F);
        if (!verifyFunction(F, &errs())) errs() << "verified\n";
        return changed ? PreservedAnalyses::none() : PreservedAnalyses::all();
    }
    // don't skip optnone
    static bool isRequired() { return true; }
};

struct OpaquePredicates : PassInfoMixin<OpaquePredicates> {
    int m_depth;
    OpaquePredicates(int depth = 1) : m_depth(depth) {}
    PreservedAnalyses run(Function &F, FunctionAnalysisManager &) {
        bool changed = false;
        for (int i = 0; i < m_depth; i++) changed |= visit_opq(F);
        if (!verifyFunction(F, &errs())) errs() << "verified\n";
        return changed ? PreservedAnalyses::none() : PreservedAnalyses::all();
    }
    static bool isRequired() { return true; }
};
} // namespace

llvm::PassPluginLibraryInfo getobf0PluginInfo() {
    return {LLVM_PLUGIN_API_VERSION, "obf0", LLVM_VERSION_STRING, [](PassBuilder &PB) {
                PB.registerOptimizerLastEPCallback([&](ModulePassManager &MPM, auto, auto) {
                    MPM.addPass(createModuleToFunctionPassAdaptor(MixedBooleanArith(1)));
                    // MPM.addPass(createModuleToFunctionPassAdaptor(OpaquePredicates(3)));
                    return true;
                });
                PB.registerPipelineParsingCallback([](StringRef PassName, FunctionPassManager &FPM, ...) {
                    FPM.addPass(MixedBooleanArith(1));
                    // FPM.addPass(OpaquePredicates(3));
                    return true;
                });
            }};
}

extern "C" LLVM_ATTRIBUTE_WEAK ::llvm::PassPluginLibraryInfo llvmGetPassPluginInfo() { return getobf0PluginInfo(); }

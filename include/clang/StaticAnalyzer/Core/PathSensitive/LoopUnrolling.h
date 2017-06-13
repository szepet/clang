//===--- LoopUnrolling.h - Unroll loops -------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// TODO: Write something nice
///
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_STATICANALYZER_CORE_PATHSENSITIVE_LOOPUNROLLING_H
#define LLVM_CLANG_STATICANALYZER_CORE_PATHSENSITIVE_LOOPUNROLLING_H

#include "clang/Analysis/CFG.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/ProgramState.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/ExplodedGraph.h"

namespace clang {
    namespace ento {

        bool shouldCompletelyUnroll(const Stmt *LoopStmt, ASTContext &ASTCtx,
                                    ExplodedNode *Pred);

    } // end namespace ento
} // end namespace clang

#endif

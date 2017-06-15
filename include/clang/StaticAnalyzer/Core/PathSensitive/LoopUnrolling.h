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
#include "clang/StaticAnalyzer/Core/PathSensitive/AnalysisManager.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/ProgramStateTrait.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/CheckerContext.h"

namespace clang {
    namespace ento {
        ProgramStateRef markBlocksAsUnrolled(const Stmt *Term,
                                             ProgramStateRef State,
                                             AnalysisManager &AMgr,
                                             CFGStmtMap *StmtToBlockMap);
        bool isUnrolledLoopBlock(const CFGBlock *Block, ProgramStateRef State);
        bool shouldCompletelyUnroll(const Stmt *LoopStmt, ASTContext &ASTCtx);

    } // end namespace ento
} // end namespace clang

#endif

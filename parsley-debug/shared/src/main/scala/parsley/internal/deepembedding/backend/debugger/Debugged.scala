/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.backend.debugger

import parsley.debugger.internal.DebugContext

import parsley.internal.deepembedding.ContOps
import parsley.internal.deepembedding.ContOps.{suspend, ContAdapter}
import parsley.internal.deepembedding.backend.{CodeGenState, StrictParsley, Unary}
import parsley.internal.deepembedding.backend.StrictParsley.InstrBuffer
import parsley.internal.deepembedding.frontend.LazyParsley
import parsley.internal.machine.instructions.{Label, Pop}
import parsley.internal.machine.instructions.debugger.{AddAttemptAndLeave, EnterParser}

private [parsley] final class Debugged[A](origin: LazyParsley[A], val p: StrictParsley[A], optName: Option[String])
    (dbgCtx: DebugContext) extends Unary[A, A] {
    override protected[backend] def codeGen[M[_, +_] : ContOps, R](producesResults: Boolean)(implicit instrs: InstrBuffer, state: CodeGenState): M[R, Unit] = {
        val handler = state.freshLabel()

        instrs += new EnterParser(handler, origin, optName)(dbgCtx)
        suspend[M, R, Unit](p.codeGen[M, R](producesResults = true)) |> {
            instrs += new Label(handler)
            instrs += new AddAttemptAndLeave(dbgCtx)
            if (!producesResults) instrs += Pop
        }
    }

    override protected def pretty(p: String): String = s"debugged($p)"
}

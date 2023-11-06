/*
 * Copyright (c) 2020, Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors
 *    may be used to endorse or promote products derived from this software without
 *    specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package parsley.internal.deepembedding.backend.debugger

import parsley.internal.deepembedding.ContOps
import parsley.internal.deepembedding.ContOps.{ContAdapter, suspend}
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

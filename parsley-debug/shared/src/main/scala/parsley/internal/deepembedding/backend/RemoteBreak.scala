/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.backend

import parsley.debug.{Breakpoint, EntryBreak, ExitBreak, FullBreak, RefCodec}
import parsley.debug.internal.DebugContext
import parsley.internal.deepembedding.ContOps
import parsley.internal.deepembedding.ContOps.{suspend, ContAdapter}
import parsley.internal.machine.instructions.debug.TriggerBreakpoint

private [deepembedding] final class InertBreak[A](p: StrictParsley[A], break: Breakpoint, refs: RefCodec*) extends StrictParsley[A] {
  override protected[backend] def codeGen[M[_, +_]: ContOps, R](producesResults: Boolean)(implicit instrs: StrictParsley.InstrBuffer, state: CodeGenState): M[R,Unit]
    = p.codeGen(producesResults)

  override private [deepembedding] def inlinable: Boolean = p.inlinable

  override private [deepembedding] def pretty: String = f"inertBreak(${p.pretty})"

  private [deepembedding] def activate(dbgCtx: DebugContext) = new ActiveBreak[A](p, break, dbgCtx, refs*)
}

private [deepembedding] final class ActiveBreak[A](p: StrictParsley[A], break: Breakpoint, dbgCtx: DebugContext, refs: RefCodec*) extends StrictParsley[A] {
  override protected[backend] def codeGen[M[_, +_]: ContOps, R](producesResults: Boolean)(implicit instrs: StrictParsley.InstrBuffer, state: CodeGenState): M[R,Unit] = {
    break match {
      case EntryBreak | FullBreak => instrs += new TriggerBreakpoint(dbgCtx, refs*)
      case _ =>
    }
    suspend[M, R, Unit](p.codeGen[M, R](producesResults)) |> {
        break match {
          case ExitBreak | FullBreak => instrs += new TriggerBreakpoint(dbgCtx, refs*)
          case _ =>
        }
    }
  }

  override private [deepembedding] def inlinable: Boolean = p.inlinable

  override private [deepembedding] def pretty: String = f"activeBreak(${p.pretty})"
}

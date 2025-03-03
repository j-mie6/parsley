/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.backend.debug

import parsley.debug.*
import parsley.debug.internal.{DebugContext, DivergenceContext}

import parsley.internal.deepembedding.ContOps
import parsley.internal.deepembedding.ContOps.{suspend, ContAdapter}
import parsley.internal.deepembedding.backend.{CodeGenState, StrictParsley, Unary}
import parsley.internal.deepembedding.backend.StrictParsley.InstrBuffer
import parsley.internal.deepembedding.frontend.LazyParsley
import parsley.internal.deepembedding.frontend.debug.{RemoteBreak, RemoteManageableBreak}
import parsley.internal.machine.instructions.{Label, Pop}
import parsley.internal.machine.instructions.debug.{AddAttemptAndLeave, DropSnapshot, EnterParser, TakeSnapshot, TriggerBreakpoint, TriggerManageableBreakpoint}

private [deepembedding] sealed abstract class TagFactory {
    def create[A](origin: LazyParsley[A], p: StrictParsley[A], isIterative: Boolean, userAssignedName: Option[String]): StrictParsley[A]
}

private [parsley] final class Debugging(dbgCtx: DebugContext) extends TagFactory {
    def create[A](origin: LazyParsley[A], p: StrictParsley[A], isIterative: Boolean, userAssignedName: Option[String]): StrictParsley[A] = {
        new Debugged(origin, p, isIterative, userAssignedName)(dbgCtx)
    }
}

private [parsley] final class CheckDivergence(dtx: DivergenceContext) extends TagFactory {
    def create[A](origin: LazyParsley[A], p: StrictParsley[A], isIterative: Boolean, userAssignedName: Option[String]): StrictParsley[A] = {
        new DivergenceChecker(origin, p, userAssignedName)(dtx)
    }
}

// backend implementations
private [backend] final class Debugged[A](origin: LazyParsley[A], val p: StrictParsley[A], isIterative: Boolean, userAssignedName: Option[String])(dbgCtx: DebugContext)
    extends Unary[A, A] {
    override protected [backend] def codeGen[M[_, +_]: ContOps, R](producesResults: Boolean)(implicit instrs: InstrBuffer, state: CodeGenState): M[R, Unit] = {
        val handler = state.freshLabel()
        instrs += new EnterParser(handler, origin, isIterative, userAssignedName)(dbgCtx)

        suspend[M, R, Unit](p.codeGen[M, R](producesResults = true)) |> {
            instrs += new Label(handler)
            origin match {
                case rb: RemoteBreak[_] => rb.break match {
                    case EntryBreak | FullBreak => instrs += new TriggerBreakpoint(dbgCtx)
                    case _ =>
                }
                case manageableRb: RemoteManageableBreak[_] => manageableRb.break match {
                    case EntryBreak | FullBreak => instrs += new TriggerManageableBreakpoint(dbgCtx, manageableRb.refs*)
                    case _ =>
                }
                case _ => instrs += new AddAttemptAndLeave(dbgCtx)
            }
            
            origin match {
                case rb: RemoteBreak[_] => rb.break match {
                    case ExitBreak | FullBreak => instrs += new TriggerBreakpoint(dbgCtx)
                    case _ =>
                }
                case _ => if (!producesResults) instrs += Pop
            }
        }
    }

    override protected def pretty(p: String): String = s"debugged($p)"
}

private [backend] final class DivergenceChecker[A](origin: LazyParsley[A], val p: StrictParsley[A], userAssignedName: Option[String])(dtx: DivergenceContext)
    extends Unary[A, A] {
    override protected [backend] def codeGen[M[_, +_]: ContOps, R](producesResults: Boolean)(implicit instrs: InstrBuffer, state: CodeGenState): M[R, Unit] = {
        val handler = state.freshLabel()
        instrs += new TakeSnapshot(handler, origin, userAssignedName)(dtx)
        suspend[M, R, Unit](p.codeGen[M, R](producesResults)) |> {
            instrs += new Label(handler)
            instrs += new DropSnapshot(dtx)
        }
    }

    override protected def pretty(p: String): String = p
}

/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.backend

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.higherKinds

import parsley.debug.{Breakpoint, EntryBreak, ExitBreak, FullBreak}
import parsley.registers.Reg

import parsley.internal.deepembedding.ContOps, ContOps.{ContAdapter, result, suspend}
import parsley.internal.deepembedding.singletons._
import parsley.internal.machine.instructions

import StrictParsley.InstrBuffer
private [deepembedding] final class Attempt[A](val p: StrictParsley[A]) extends ScopedUnaryWithState[A, A](false) {
    override val instr: instructions.Instr = instructions.PopHandlerAndState
    override def instrNeedsLabel: Boolean = false
    override def handlerLabel(state: CodeGenState): Int  = state.getLabel(instructions.RestoreAndFail)
    override def optimise: StrictParsley[A] = p match {
        case p: CharTok => p
        case p: Attempt[_] => p
        case StringTok(s) if s.size == 1 => p
        case _ => this
    }
}
private [deepembedding] final class Look[A](val p: StrictParsley[A]) extends ScopedUnaryWithState[A, A](true) {
    override val instr: instructions.Instr = instructions.RestoreHintsAndState
    override def instrNeedsLabel: Boolean = false
    override def handlerLabel(state: CodeGenState): Int  = state.getLabel(instructions.PopStateAndFail)
}
private [deepembedding] final class NotFollowedBy[A](val p: StrictParsley[A]) extends Unary[A, Unit] {
    override def optimise: StrictParsley[Unit] = p match {
        case z: MZero => new Pure(())
        case _        => this
    }
    final override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        val handler = state.freshLabel()
        instrs += new instructions.PushHandlerAndState(handler, saveHints = true, hideHints = true)
        suspend[Cont, R, Unit](p.codeGen) |> {
            instrs += instructions.NegLookFail
            instrs += new instructions.Label(handler)
            instrs += instructions.NegLookGood
        }
    }
}

private [deepembedding] final class Rec[A](val call: instructions.Call) extends StrictParsley[A] with Binding {
    // Must be a def, since call.label can change!
    def inlinable: Boolean = true
    def label: Int = call.label

    final override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = result(instrs += call)
}
private [deepembedding] final class Let[A](val p: StrictParsley[A]) extends StrictParsley[A] with Binding {
    def inlinable: Boolean = true
    def label(implicit state: CodeGenState): Int = state.getLabel(this)
    override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        result(instrs += new instructions.Call(label))
    }
}
private [deepembedding] final class Put[S](reg: Reg[S], val p: StrictParsley[S]) extends Unary[S, Unit] {
    override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        suspend(p.codeGen[Cont, R]) |>
        (instrs += new instructions.Put(reg.addr))
    }
}

// $COVERAGE-OFF$
private [deepembedding] final class Debug[A](val p: StrictParsley[A], name: String, ascii: Boolean, break: Breakpoint) extends Unary[A, A] {
    override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        val handler = state.freshLabel()
        instrs += new instructions.LogBegin(handler, name, ascii, (break eq EntryBreak) || (break eq FullBreak))
        suspend(p.codeGen[Cont, R]) |> {
            instrs += new instructions.Label(handler)
            instrs += new instructions.LogEnd(name, ascii, (break eq ExitBreak) || (break eq FullBreak))
        }
    }
}
// $COVERAGE-ON$

private [backend] object Attempt {
    def unapply[A](self: Attempt[A]): Some[StrictParsley[A]] = Some(self.p)
}

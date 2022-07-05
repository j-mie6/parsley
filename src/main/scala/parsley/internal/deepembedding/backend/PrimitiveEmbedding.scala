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
    // $COVERAGE-OFF$
    final override def pretty(p: String): String = s"attempt($p)"
    // $COVERAGE-ON$
}
private [deepembedding] final class Look[A](val p: StrictParsley[A]) extends ScopedUnaryWithState[A, A](true) {
    override val instr: instructions.Instr = instructions.RestoreHintsAndState
    override def instrNeedsLabel: Boolean = false
    override def handlerLabel(state: CodeGenState): Int  = state.getLabel(instructions.PopStateAndFail)
    // $COVERAGE-OFF$
    final override def pretty(p: String): String = s"lookAhead($p)"
    // $COVERAGE-ON$
}
private [deepembedding] final class NotFollowedBy[A](val p: StrictParsley[A]) extends Unary[A, Unit] {
    override def optimise: StrictParsley[Unit] = p match {
        case z: MZero => new Pure(())
        case _        => this
    }
    final override def codeGen[Cont[_, +_]: ContOps, R](implicit instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        val handler = state.freshLabel()
        instrs += new instructions.PushHandlerAndState(handler, saveHints = true, hideHints = true)
        suspend[Cont, R, Unit](p.codeGen) |> {
            instrs += instructions.NegLookFail
            instrs += new instructions.Label(handler)
            instrs += instructions.NegLookGood
        }
    }
    // $COVERAGE-OFF$
    final override def pretty(p: String): String = s"notFollowedBy($p)"
    // $COVERAGE-ON$
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
    override def codeGen[Cont[_, +_]: ContOps, R](implicit instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        result(instrs += new instructions.Call(label))
    }
}
private [deepembedding] final class Put[S](reg: Reg[S], val p: StrictParsley[S]) extends Unary[S, Unit] {
    override def codeGen[Cont[_, +_]: ContOps, R](implicit instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        suspend(p.codeGen[Cont, R]) |>
        (instrs += new instructions.Put(reg.addr))
    }
    // $COVERAGE-OFF$
    final override def pretty(p: String): String = s"put(r${reg.addr}, $p)"
    // $COVERAGE-ON$
}

private [deepembedding] final class NewReg[S, A](reg: Reg[S], init: StrictParsley[S], body: StrictParsley[A]) extends StrictParsley[A] {
    def inlinable: Boolean = false
    override def codeGen[Cont[_, +_]: ContOps, R](implicit instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        val handler = state.getLabelForPutAndFail(reg)
        suspend(init.codeGen[Cont, R]) >> {
            instrs += new instructions.Get(reg.addr)
            instrs += new instructions.SwapAndPut(reg.addr)
            instrs += new instructions.PushHandler(handler)
            suspend(body.codeGen[Cont, R]) |> {
                instrs += new instructions.SwapAndPut(reg.addr)
                instrs += instructions.PopHandler
            }
        }
    }
    // $COVERAGE-OFF$
    final override def pretty[Cont[_, +_]: ContOps, R]: Cont[R,String] =
        for {
            s1 <- init.pretty
            s2 <- body.pretty
        } yield s"newreg(r${reg.addr}, $s1, $s2)"
    // $COVERAGE-ON$
}

// $COVERAGE-OFF$
private [deepembedding] final class Debug[A](val p: StrictParsley[A], name: String, ascii: Boolean, break: Breakpoint) extends Unary[A, A] {
    override def codeGen[Cont[_, +_]: ContOps, R](implicit instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        val handler = state.freshLabel()
        instrs += new instructions.LogBegin(handler, name, ascii, (break eq EntryBreak) || (break eq FullBreak))
        suspend(p.codeGen[Cont, R]) |> {
            instrs += new instructions.Label(handler)
            instrs += new instructions.LogEnd(name, ascii, (break eq ExitBreak) || (break eq FullBreak))
        }
    }
    final override def pretty(p: String): String = p
}
private [deepembedding] final class DebugError[A](val p: StrictParsley[A], name: String, ascii: Boolean) extends Unary[A, A] {
    override def codeGen[Cont[_, +_]: ContOps, R](implicit instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        val handler = state.freshLabel()
        instrs += new instructions.LogErrBegin(handler, name, ascii)
        suspend(p.codeGen[Cont, R]) |> {
            instrs += new instructions.Label(handler)
            instrs += new instructions.LogErrEnd(name, ascii)
        }
    }
    final override def pretty(p: String): String = p
}
// $COVERAGE-ON$

private [backend] object Attempt {
    def unapply[A](self: Attempt[A]): Some[StrictParsley[A]] = Some(self.p)
}

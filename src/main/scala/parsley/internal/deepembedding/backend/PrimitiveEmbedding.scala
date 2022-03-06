package parsley.internal.deepembedding.backend

import parsley.internal.deepembedding.ContOps, ContOps.{result, suspend, ContAdapter}
import parsley.internal.deepembedding.singletons._
import parsley.internal.machine.instructions
import parsley.registers.Reg
import parsley.debug.{Breakpoint, EntryBreak, FullBreak, ExitBreak}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.higherKinds
import StrictParsley.InstrBuffer
private [deepembedding] final class Attempt[A](val p: StrictParsley[A]) extends ScopedUnaryWithState[A, A](false) {
    override val instr: instructions.Instr = instructions.Attempt
}
private [deepembedding] final class Look[A](val p: StrictParsley[A]) extends ScopedUnaryWithState[A, A](true) {
    override val instr: instructions.Instr = instructions.Look
}
private [deepembedding] final class NotFollowedBy[A](val p: StrictParsley[A]) extends ScopedUnaryWithState[A, Unit](true) {
    override val instr: instructions.Instr = instructions.NotFollowedBy
    override def optimise: StrictParsley[Unit] = p match {
        case z: MZero => new Pure(())
        case _ => this
    }
}

private [deepembedding] final class Rec[A](val call: instructions.Call) extends StrictParsley[A] with Binding {
    // Must be a def, since call.label can change!
    def label: Int = call.label
    // $COVERAGE-OFF$
    // This is here because Scala needs it to be, it's not used
    def preserve: Array[Int] = call.preserve
    // $COVERAGE-ON$
    def preserve_=(indices: Array[Int]): Unit = call.preserve = indices

    def inlinable: Boolean = true

    final override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = result(instrs += call)
}
private [deepembedding] final class Let[A](val p: StrictParsley[A]) extends StrictParsley[A] with Binding {
    def inlinable: Boolean = true
    def label(implicit state: CodeGenState): Int = state.getLabel(this)
    override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        result(instrs += new instructions.GoSub(label))
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
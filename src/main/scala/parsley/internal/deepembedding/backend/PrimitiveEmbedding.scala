package parsley.internal.deepembedding.backend

import parsley.internal.deepembedding.ContOps, ContOps.{result, ContAdapter}
import parsley.internal.machine.instructions
import parsley.registers.Reg
import parsley.debug.{Breakpoint, EntryBreak, FullBreak, ExitBreak}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.higherKinds
import StrictParsley.InstrBuffer

private [parsley] final class Satisfy(private [Satisfy] val f: Char => Boolean, val expected: Option[String]) extends Singleton[Char](new instructions.Satisfies(f, expected))

private [parsley] final class Attempt[A](var p: StrictParsley[A]) extends ScopedUnaryWithState[A, A](false, instructions.Attempt)
private [parsley] final class Look[A](var p: StrictParsley[A]) extends ScopedUnaryWithState[A, A](true, instructions.Look)
private [parsley] final class NotFollowedBy[A](var p: StrictParsley[A])
    extends ScopedUnaryWithState[A, Unit](true, instructions.NotFollowedBy) {
    override def optimise: StrictParsley[Unit] = p match {
        case z: MZero => new Pure(())
        case _ => this
    }
}

private [deepembedding] final class Rec[A](val call: instructions.Call) extends Singleton(call) with Binding {
    // Must be a def, since call.label can change!
    def label: Int = call.label
    // $COVERAGE-OFF$
    // This is here because Scala needs it to be, it's not used
    def preserve: Array[Int] = call.preserve
    // $COVERAGE-ON$
    def preserve_=(indices: Array[Int]): Unit = call.preserve = indices
}
private [deepembedding] final class Let[A](val p: StrictParsley[A]) extends StrictParsley[A] with Binding {
    val size = 1
    def label(implicit state: CodeGenState): Int = state.getLabel(this)
    override def optimise: StrictParsley[A] = if (p.size <= 1) p else this
    override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont, R], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        result(instrs += new instructions.GoSub(label))
    }
}

private [parsley] object Line extends Singleton[Int](instructions.Line)
private [parsley] object Col extends Singleton[Int](instructions.Col)
private [parsley] final class Get[S](reg: Reg[S]) extends Singleton[S](new instructions.Get(reg.addr))
private [parsley] final class Put[S](reg: Reg[S], var p: StrictParsley[S]) extends Unary[S, Unit] {
    override val numInstrs = 1
    override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont, R], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        p.codeGen |>
        (instrs += new instructions.Put(reg.addr))
    }
}

// $COVERAGE-OFF$
private [parsley] final class Debug[A](var p: StrictParsley[A], name: String, ascii: Boolean, break: Breakpoint) extends Unary[A, A] {
    override val numInstrs = 2
    override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont, R], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        val handler = state.freshLabel()
        instrs += new instructions.LogBegin(handler, name, ascii, (break eq EntryBreak) || (break eq FullBreak))
        p.codeGen |> {
            instrs += new instructions.Label(handler)
            instrs += new instructions.LogEnd(name, ascii, (break eq ExitBreak) || (break eq FullBreak))
        }
    }
}
// $COVERAGE-ON$

private [deepembedding] object Satisfy {
    def unapply(self: Satisfy): Some[Char => Boolean] = Some(self.f)
}
private [deepembedding] object Attempt {
    def unapply[A](self: Attempt[A]): Some[StrictParsley[A]] = Some(self.p)
}
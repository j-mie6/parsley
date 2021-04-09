package parsley.internal.deepembedding

import ContOps.{result, ContAdapter}
import parsley.internal.machine.instructions
import parsley.registers.Reg
import parsley.debug.{Breakpoint, EntryBreak, FullBreak, ExitBreak}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.higherKinds

private [parsley] final class Satisfy(private [Satisfy] val f: Char => Boolean, val expected: Option[String])
    extends Singleton[Char]("satisfy(f)", new instructions.Satisfies(f, expected))

private [parsley] final class Attempt[A](_p: =>Parsley[A]) extends ScopedUnaryWithState[A, A](_p, "attempt", false, Attempt.empty, instructions.Attempt)
private [parsley] final class Look[A](_p: =>Parsley[A]) extends ScopedUnaryWithState[A, A](_p, "lookAhead", true, Look.empty, instructions.Look)
private [parsley] final class NotFollowedBy[A](_p: =>Parsley[A])
    extends ScopedUnaryWithState[A, Unit](_p, "notFollowedBy", true, NotFollowedBy.empty, instructions.NotFollowedBy) {
    override def optimise: Parsley[Unit] = p match {
        case z: MZero => new Pure(())
        case _ => this
    }
}

private [deepembedding] final class Rec[A](private [deepembedding] val p: Parsley[A], val call: instructions.Call)
    extends Singleton(s"rec($p)", call) with Binding {
    def label: Int = call.label
    // $COVERAGE-OFF$
    // This is here because Scala needs it to be, it's not used
    def preserve: Array[Int] = call.preserve
    // $COVERAGE-ON$
    def preserve_=(indices: Array[Int]): Unit = call.preserve = indices
}
private [deepembedding] final class Let[A](var p: Parsley[A]) extends Parsley[A] with Binding {
    def label(implicit state: CodeGenState): Int = state.getLabel(this)
    // $COVERAGE-OFF$
    override def findLetsAux[Cont[_, +_], R](implicit ops: ContOps[Cont, R], seen: Set[Parsley[_]], state: LetFinderState): Cont[R, Unit] = {
        throw new Exception("Lets cannot exist during let detection")
    }
    // $COVERAGE-ON$
    override def preprocess[Cont[_, +_], R, A_ >: A](implicit ops: ContOps[Cont, R], seen: Set[Parsley[_]], sub: LetMap, recs: RecMap): Cont[R, Parsley[A_]] = {
        for (p <- this.p.optimised) yield this.ready(p)
    }
    private def ready(p: Parsley[A]): this.type = {
        this.p = p
        processed = true
        this
    }
    override def optimise: Parsley[A] = if (p.size <= 1) p else this
    override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont, R], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        result(instrs += new instructions.GoSub(label))
    }
    // $COVERAGE-OFF$
    override def prettyASTAux[Cont[_, +_], R](implicit ops: ContOps[Cont, R]): Cont[R, String] = result(s"Sub($p)")
    // $COVERAGE-ON$
}

private [parsley] object Line extends Singleton[Int]("line", instructions.Line)
private [parsley] object Col extends Singleton[Int]("col", instructions.Col)
private [parsley] final class Get[S](val reg: Reg[S]) extends Singleton[S](s"get($reg)", new instructions.Get(reg.addr)) with UsesRegister
private [parsley] final class Put[S](val reg: Reg[S], _p: =>Parsley[S])
    extends Unary[S, Unit](_p)(c => s"put($reg, $c)", Put.empty(reg)) with UsesRegister {
    override val numInstrs = 1
    override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont, R], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        p.codeGen |>
        (instrs += new instructions.Put(reg.addr))
    }
}

// $COVERAGE-OFF$
private [parsley] final class Debug[A](_p: =>Parsley[A], name: String, ascii: Boolean, break: Breakpoint)
    extends Unary[A, A](_p)(identity[String], Debug.empty(name, ascii, break)) {
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
    def empty[A]: Attempt[A] = new Attempt(???)
    def unapply[A](self: Attempt[A]): Some[Parsley[A]] = Some(self.p)
}
private [deepembedding] object Look {
    def empty[A]: Look[A] = new Look(???)
}
private [deepembedding] object NotFollowedBy {
    def empty[A]: NotFollowedBy[A] = new NotFollowedBy(???)
}
private [deepembedding] object Put {
    def empty[S](r: Reg[S]): Put[S] = new Put(r, ???)
}
// $COVERAGE-OFF$
private [deepembedding] object Debug {
    def empty[A](name: String, ascii: Boolean, break: Breakpoint): Debug[A] = new Debug(???, name, ascii, break)
}
// $COVERAGE-ON$
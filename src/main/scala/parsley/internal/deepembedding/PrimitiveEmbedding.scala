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

private [deepembedding] sealed abstract class ScopedUnary[A, B](_p: =>Parsley[A], name: String, doesNotProduceHints: Boolean,
                                                                empty: =>ScopedUnary[A, B], instr: instructions.Instr)
    extends Unary[A, B](_p)(c => s"$name($c)", empty) {
    final override val numInstrs = 2
    final override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont, R], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        val handler = state.freshLabel()
        instrs += new instructions.PushHandlerAndState(handler, doesNotProduceHints, doesNotProduceHints)
        p.codeGen |> {
            instrs += new instructions.Label(handler)
            instrs += instr
        }
    }
}
private [parsley] final class Attempt[A](_p: =>Parsley[A]) extends ScopedUnary[A, A](_p, "attempt", false, Attempt.empty, instructions.Attempt)
private [parsley] final class Look[A](_p: =>Parsley[A]) extends ScopedUnary[A, A](_p, "lookAhead", true, Look.empty, instructions.Look)
private [parsley] final class NotFollowedBy[A](_p: =>Parsley[A])
    extends ScopedUnary[A, Unit](_p, "notFollowedBy", true, NotFollowedBy.empty, instructions.NotFollowedBy) {
    override def optimise: Parsley[Unit] = p match {
        case z: MZero => new Pure(())
        case _ => this
    }
}

private [parsley] final class Fail(private [Fail] val msg: String)
    extends Singleton[Nothing](s"fail($msg)", new instructions.Fail(msg)) with MZero

private [parsley] final class Unexpected(private [Unexpected] val msg: String)
    extends Singleton[Nothing](s"unexpected($msg)", new instructions.Unexpected(msg)) with MZero

private [deepembedding] final class Rec[A](private [deepembedding] val p: Parsley[A], private [deepembedding] val label: Int, val call: instructions.Call)
    extends Singleton(s"rec($p)", call)
private [deepembedding] final class Subroutine[A](var p: Parsley[A]) extends Parsley[A] {
    // $COVERAGE-OFF$
    override def findLetsAux[Cont[_, +_], R](implicit ops: ContOps[Cont, R], seen: Set[Parsley[_]], state: LetFinderState): Cont[R, Unit] = {
        throw new Exception("Subroutines cannot exist during let detection")
    }
    // $COVERAGE-ON$
    override def preprocess[Cont[_, +_], R, A_ >: A](implicit ops: ContOps[Cont, R], seen: Set[Parsley[_]], sub: SubMap, recs: RecMap[Cont]): Cont[R, Parsley[A_]] = {
        for (p <- this.p.optimised) yield this.ready(p)
    }
    private def ready(p: Parsley[A]): this.type = {
        this.p = p
        processed = true
        this
    }
    override def optimise: Parsley[A] = if (p.size <= 1) p else this
    override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont, R], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        result(instrs += new instructions.GoSub(state.getLabel(this)))
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

private [parsley] final class ErrorLabel[A](_p: =>Parsley[A], private [ErrorLabel] val label: String)
    extends Unary[A, A](_p)(c => s"$c.label($label)", ErrorLabel.empty(label)) {
    final override val numInstrs = 2
    final override def optimise: Parsley[A] = p match {
        case ct@CharTok(c) if !ct.expected.contains("") => new CharTok(c, Some(label)).asInstanceOf[Parsley[A]]
        case st@StringTok(s) if !st.expected.contains("") => new StringTok(s, Some(label)).asInstanceOf[Parsley[A]]
        case sat@Satisfy(f) if !sat.expected.contains("") => new Satisfy(f, Some(label)).asInstanceOf[Parsley[A]]
        case ErrorLabel(p, label2) if label2 != "" => ErrorLabel(p, label)
        case _ => this
    }
    final override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont, R], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        val handler = state.freshLabel()
        instrs += new instructions.InputCheck(handler, true)
        p.codeGen |> {
            instrs += new instructions.Label(handler)
            instrs += new instructions.ApplyError(label)
        }
    }
}
private [parsley] final class ErrorExplain[A](_p: =>Parsley[A], reason: String)
    extends Unary[A, A](_p)(c => s"$c.explain($reason)", ErrorExplain.empty(reason)) {
    final override val numInstrs = 2
    final override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont, R], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        val handler = state.freshLabel()
        instrs += new instructions.InputCheck(handler)
        p.codeGen |> {
            instrs += new instructions.Label(handler)
            instrs += new instructions.ApplyReason(reason)
        }
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
private [deepembedding] object ErrorLabel {
    def empty[A](label: String): ErrorLabel[A] = new ErrorLabel(???, label)
    def apply[A](p: Parsley[A], label: String): ErrorLabel[A] = empty(label).ready(p)
    def unapply[A](self: ErrorLabel[A]): Some[(Parsley[A], String)] = Some((self.p, self.label))
}
private [deepembedding] object ErrorExplain {
    def empty[A](reason: String): ErrorExplain[A] = new ErrorExplain(???, reason)
    def apply[A](p: Parsley[A], reason: String): ErrorExplain[A] = empty(reason).ready(p)
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
package parsley.internal.deepembedding.backend

import parsley.internal.deepembedding.ContOps, ContOps.{result, ContAdapter}
import parsley.internal.machine.instructions

import scala.language.higherKinds
import StrictParsley.InstrBuffer

import Branch.FlipApp

private [deepembedding] sealed abstract class BranchLike[A, B, C, D](instr: Int => instructions.Instr, finaliser: Option[instructions.Instr]) extends StrictParsley[D] {
    val b: StrictParsley[A]
    val p: StrictParsley[B]
    val q: StrictParsley[C]
    val size = b.size + p.size + q.size + 2 + finaliser.size * 2
    final override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont, R], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        val toP = state.freshLabel()
        val end = state.freshLabel()
        b.codeGen >> {
            instrs += instr(toP)
            q.codeGen >> {
                for (instr <- finaliser) instrs += instr
                instrs += new instructions.Jump(end)
                instrs += new instructions.Label(toP)
                p.codeGen |> {
                    for (instr <- finaliser) instrs += instr
                    instrs += new instructions.Label(end)
                }
            }
        }
    }
}

private [parsley] final class Branch[A, B, C](val b: StrictParsley[Either[A, B]], val p: StrictParsley[A => C], val q: StrictParsley[B => C])
    extends BranchLike[Either[A, B], A => C, B => C, C](new instructions.Case(_), Some(FlipApp)) {

    override def optimise: StrictParsley[C] = b match {
        case Pure(Left(x)) => <*>(p, new Pure(x)).optimise
        case Pure(Right(y)) => <*>(q, new Pure(y)).optimise
        case _ => (p, q) match {
            case (Pure(f), Pure(g)) => <*>(new Pure((x: Either[A, B]) => x.fold(f, g)), b)
            case _ => this
        }
    }
}

private [parsley] final class If[A](val b: StrictParsley[Boolean], val p: StrictParsley[A], val q: StrictParsley[A]) extends BranchLike[Boolean, A, A, A](new instructions.If(_), None) {
    override def optimise: StrictParsley[A] = b match {
        case Pure(true) => p
        case Pure(false) => q
        case _ => this
    }
}

private [deepembedding] sealed abstract class FastZero[A](fail: A => StrictParsley[Nothing], instr: instructions.Instr) extends Unary[A, Nothing] {
    final override val numInstrs = 1
    final override def optimise: StrictParsley[Nothing] = p match {
        case Pure(x) => fail(x)
        case z: MZero => z
        case _ => this
    }
    final override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont, R], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        p.codeGen |> (instrs += instr)
    }
}
private [parsley] final class FastFail[A](var p: StrictParsley[A], msggen: A => String) extends FastZero[A](x => new Fail(msggen(x)), new instructions.FastFail(msggen)) with MZero
private [parsley] final class FastUnexpected[A](var p: StrictParsley[A], msggen: A => String) extends FastZero[A](x => new Unexpected(msggen(x)), new instructions.FastUnexpected(msggen)) with MZero

private [deepembedding] sealed abstract class FilterLike[A](fail: A => StrictParsley[Nothing], instr: instructions.Instr, pred: A => Boolean) extends Unary[A, A] {
    final override val numInstrs = 1
    final override def optimise: StrictParsley[A] = p match {
        case Pure(x) if pred(x) => fail(x)
        case px: Pure[_] => px
        case z: MZero => z
        case _ => this
    }
    final override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont, R], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        p.codeGen |> (instrs += instr)
    }
}
private [parsley] final class Filter[A](var p: StrictParsley[A], pred: A => Boolean) extends FilterLike[A](_ => Empty, new instructions.Filter(pred), !pred(_))
private [parsley] final class FilterOut[A](var p: StrictParsley[A], pred: PartialFunction[A, String]) extends FilterLike[A](x => ErrorExplain(Empty, pred(x)), new instructions.FilterOut(pred), pred.isDefinedAt(_))
private [parsley] final class GuardAgainst[A](var p: StrictParsley[A], pred: PartialFunction[A, String]) extends FilterLike[A](x => new Fail(pred(x)), new instructions.GuardAgainst(pred), pred.isDefinedAt(_))

private [deepembedding] object Branch {
    val FlipApp = new instructions.Lift2[Any, Any => Any, Any]((x, f) => f(x))
}
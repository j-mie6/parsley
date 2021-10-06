package parsley.internal.deepembedding

import ContOps.{result, ContAdapter}
import parsley.internal.machine.instructions

import scala.language.higherKinds

import Branch.FlipApp

private [deepembedding] sealed abstract class BranchLike[A, B, C, D](_b: Parsley[A], _p: =>Parsley[B], _q: =>Parsley[C],
                                                                     pretty: (String, String, String) => String, make: Parsley[A] => BranchLike[A, B, C, D],
                                                                     instr: Int => instructions.Instr, finaliser: Option[instructions.Instr])
    extends Ternary[A, B, C, D](_b, _p, _q, pretty, make) {
    final override val numInstrs = 2
    final override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont, R], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        val toSecond = state.freshLabel()
        val end = state.freshLabel()
        first.codeGen >> {
            instrs += instr(toSecond)
            third.codeGen >> {
                for (instr <- finaliser) instrs += instr
                instrs += new instructions.Jump(end)
                instrs += new instructions.Label(toSecond)
                second.codeGen |> {
                    for (instr <- finaliser) instrs += instr
                    instrs += new instructions.Label(end)
                }
            }
        }
    }
}

private [parsley] final class Branch[A, B, C](_b: Parsley[Either[A, B]], _p: =>Parsley[A => C], _q: =>Parsley[B => C])
    extends BranchLike[Either[A, B], A => C, B => C, C](_b, _p, _q, (f, s, t) => s"branch($f, $s, $t)",
                                                        new Branch(_, ???, ???), new instructions.Case(_), Some(FlipApp)) {

    override def optimise: Parsley[C] = first match {
        case Pure(Left(x)) => <*>(second, new Pure(x)).optimise
        case Pure(Right(y)) => <*>(third, new Pure(y)).optimise
        case _ => (second, third) match {
            case (Pure(f), Pure(g)) => <*>(new Pure((x: Either[A, B]) => x.fold(f, g)), first)
            case _ => this
        }
    }
}

private [parsley] final class If[A](_b: Parsley[Boolean], _p: =>Parsley[A], _q: =>Parsley[A])
    extends BranchLike[Boolean, A, A, A](_b, _p, _q, (f, s, t) => s"($f ? $s : $t)", new If(_, ???, ???), new instructions.If(_), None) {
    override def optimise: Parsley[A] = first match {
        case Pure(true) => second
        case Pure(false) => third
        case _ => this
    }
}

private [deepembedding] sealed abstract class FilterLike[A, B](_p: Parsley[A], pretty: String => String, make: Parsley[A] => FilterLike[A, B],
                                                               fail: A => Parsley[B], instr: instructions.Instr, pred: A => Boolean)
    extends Unary[A, B](_p, pretty, make) {
    final override val numInstrs = 1
    final override def optimise: Parsley[B] = p match {
        case px@Pure(x) => if (!pred(x)) px.asInstanceOf[Parsley[B]] else fail(x)
        case z: MZero => z
        case _ => this
    }
    final override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont, R], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        p.codeGen |> (instrs += instr)
    }
}
private [parsley] final class FastFail[A](_p: Parsley[A], msggen: A => String)
    extends FilterLike[A, Nothing](_p, c => s"$c ! ?", new FastFail(_, msggen),
                                   x => new Fail(msggen(x)), new instructions.FastFail(msggen), _ => true) with MZero
private [parsley] final class FastUnexpected[A](_p: Parsley[A], msggen: A => String)
    extends FilterLike[A, Nothing](_p, c => s"$c.unexpected(?)", new FastUnexpected(_, msggen),
                                   x => new Unexpected(msggen(x)), new instructions.FastUnexpected(msggen), _ => true) with MZero
private [parsley] final class Filter[A](_p: Parsley[A], pred: A => Boolean)
    extends FilterLike[A, A](_p, c => s"$c.filter(?)", new Filter(_, pred),
                             _ => Empty, new instructions.Filter(pred), !pred(_))
private [parsley] final class FilterOut[A](_p: Parsley[A], pred: PartialFunction[A, String])
    extends FilterLike[A, A](_p, c => s"$c.filterOut(?)", new FilterOut(_, pred),
                             x => ErrorExplain(Empty, pred(x)), new instructions.FilterOut(pred), pred.isDefinedAt(_))
private [parsley] final class GuardAgainst[A](_p: Parsley[A], pred: PartialFunction[A, String])
    extends FilterLike[A, A](_p, c => s"$c.guardAgainst(?)", new GuardAgainst(_, pred),
                            x => new Fail(pred(x)), new instructions.GuardAgainst(pred), pred.isDefinedAt(_))

private [deepembedding] object Branch {
    val FlipApp = new instructions.Lift2[Any, Any => Any, Any]((x, f) => f(x))
}
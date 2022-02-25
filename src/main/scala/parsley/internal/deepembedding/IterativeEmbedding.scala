package parsley.internal.deepembedding

import ContOps.{result, ContAdapter}
import parsley.internal.machine.instructions

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.higherKinds

import backend.StrictParsley

private [deepembedding] sealed abstract class ManyLike[A, B](_p: Parsley[A], name: String, make: StrictParsley[A] => ManyLike[A, B],
                                                             unit: B, instr: Int => instructions.Instr)
    extends Unary[A, B](_p, c => s"$name($c)", make) {
    final override val numInstrs = 2
    final override def optimise: Parsley[B] = p match {
        case _: Pure[_] => throw new Exception(s"$name given parser which consumes no input")
        case _: MZero => new Pure(unit)
        case _ => this
    }
    final override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont, R], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        val body = state.freshLabel()
        val handler = state.freshLabel()
        instrs += new instructions.InputCheck(handler)
        instrs += new instructions.Label(body)
        p.codeGen |> {
            instrs += new instructions.Label(handler)
            instrs += instr(body)
        }
    }
}
private [parsley] final class Many[A](_p: StrictParsley[A]) extends ManyLike[A, List[A]](_p.asInstanceOf, "many", new Many(_), Nil, new instructions.Many(_))
private [parsley] final class SkipMany[A](_p: StrictParsley[A]) extends ManyLike[A, Unit](_p.asInstanceOf, "skipMany", new SkipMany(_), (), new instructions.SkipMany(_))
private [deepembedding] sealed abstract class ChainLike[A](p: Parsley[A], _op: =>Parsley[A => A],
                                                           pretty: (String, String) => String, make: StrictParsley[A] => ChainLike[A])
    extends Binary[A, A => A, A](p, _op, pretty, make) {
    override def optimise: Parsley[A] = right match {
        case _: Pure[_] => throw new Exception("chain given parser which consumes no input")
        case _: MZero => left
        case _ => this
    }
}
private [parsley] final class ChainPost[A](p: StrictParsley[A], _op: =>Parsley[A => A])
    extends ChainLike[A](p.asInstanceOf, _op, (l, r) => s"chainPost($l, $r)", new ChainPost(_, ???)) {
    override val numInstrs = 2
    override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont, R], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        val body = state.freshLabel()
        val handler = state.freshLabel()
        left.codeGen >> {
            instrs += new instructions.InputCheck(handler)
            instrs += new instructions.Label(body)
            right.codeGen |> {
                instrs += new instructions.Label(handler)
                instrs += new instructions.ChainPost(body)
            }
        }
    }
}
// This can't be fully strict, because it depends on binary!
private [parsley] final class ChainPre[A](p: StrictParsley[A], _op: =>Parsley[A => A])
    extends ChainLike[A](p.asInstanceOf, _op, (l, r) => s"chainPre($r, $l)", new ChainPre(_, ???)) {
    override val numInstrs = 3
    override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont, R], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        val body = state.freshLabel()
        val handler = state.freshLabel()
        instrs += new instructions.InputCheck(handler)
        instrs += new instructions.Label(body)
        right.codeGen >> {
            instrs += new instructions.Label(handler)
            instrs += new instructions.ChainPre(body)
            left.codeGen |>
            (instrs += instructions.Apply)
        }
    }
}
private [parsley] final class Chainl[A, B](init: StrictParsley[B], _p: =>Parsley[A], _op: =>Parsley[(B, A) => B])
    extends Ternary[B, A, (B, A) => B, B](init.asInstanceOf, _p, _op, (f, s, t) => s"chainl1($s, $t)", new Chainl(_, ???, ???)) {
    override val numInstrs = 2
    override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont, R], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        val body = state.freshLabel()
        val handler = state.freshLabel()
        first.codeGen >> {
            instrs += new instructions.InputCheck(handler)
            instrs += new instructions.Label(body)
            third.codeGen >>
            second.codeGen |> {
                instrs += new instructions.Label(handler)
                instrs += new instructions.Chainl(body)
            }
        }
    }
}
private [parsley] final class Chainr[A, B](p: StrictParsley[A], _op: =>Parsley[(A, B) => B], private [Chainr] val wrap: A => B)
    extends Binary[A, (A, B) => B, B](p.asInstanceOf, _op, (l, r) => s"chainr1($l, $r)", new Chainr(_, ???, wrap)) {
    override val numInstrs = 3
    override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont, R], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit]= {
        val body = state.freshLabel()
        val handler = state.freshLabel()
        instrs += new instructions.InputCheck(handler)
        instrs += new instructions.Label(body)
        left.codeGen >> {
            instrs += new instructions.InputCheck(handler)
            right.codeGen |> {
                instrs += new instructions.Label(handler)
                instrs += new instructions.Chainr(body, wrap)
            }
        }
    }
}
private [parsley] final class SepEndBy1[A, B](p: StrictParsley[A], _sep: =>Parsley[B])
    extends Binary[A, B, List[A]](p.asInstanceOf, _sep, (l, r) => s"sepEndBy1($r, $l)", new SepEndBy1(_, ???)) {
    override val numInstrs = 3
    override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont, R], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        val body = state.freshLabel()
        val handler = state.freshLabel()
        instrs += new instructions.InputCheck(handler)
        instrs += new instructions.Label(body)
        left.codeGen >> {
            instrs += new instructions.InputCheck(handler)
            right.codeGen |> {
                instrs += new instructions.Label(handler)
                instrs += new instructions.SepEndBy1(body)
            }
        }
    }
}
private [parsley] final class ManyUntil[A](body: StrictParsley[Any]) extends Unary[Any, List[A]](body.asInstanceOf, c => s"manyUntil($c)", new ManyUntil(_)) {
    override val numInstrs = 2
    override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont, R], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        val start = state.freshLabel()
        val loop = state.freshLabel()
        instrs += new instructions.PushHandler(loop)
        instrs += new instructions.Label(start)
        p.codeGen |> {
            instrs += new instructions.Label(loop)
            instrs += new instructions.ManyUntil(start)
        }
    }
}

private [parsley] object ManyUntil {
    object Stop
}
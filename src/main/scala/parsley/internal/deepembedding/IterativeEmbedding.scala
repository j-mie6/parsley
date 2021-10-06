package parsley.internal.deepembedding

import ContOps.{result, ContAdapter}
import parsley.internal.machine.instructions

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.higherKinds

private [deepembedding] sealed abstract class ManyLike[A, B](_p: Parsley[A], name: String, make: Parsley[A] => ManyLike[A, B], unit: B, instr: Int => instructions.Instr)
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
private [parsley] final class Many[A](_p: Parsley[A]) extends ManyLike[A, List[A]](_p, "many", new Many(_), Nil, new instructions.Many(_))
private [parsley] final class SkipMany[A](_p: Parsley[A]) extends ManyLike[A, Unit](_p, "skipMany", new SkipMany(_), (), new instructions.SkipMany(_))
private [deepembedding] sealed abstract class ChainLike[A](_p: =>Parsley[A], _op: =>Parsley[A => A], pretty: (String, String) => String, empty: =>ChainLike[A])
    extends Binary[A, A => A, A](_p, _op, pretty, empty) {
    override def optimise: Parsley[A] = right match {
        case _: Pure[_] => throw new Exception("chain given parser which consumes no input")
        case _: MZero => left
        case _ => this
    }
}
private [parsley] final class ChainPost[A](_p: =>Parsley[A], _op: =>Parsley[A => A])
    extends ChainLike[A](_p, _op, (l, r) => s"chainPost($l, $r)", new ChainPost(???, ???)) {
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
private [parsley] final class ChainPre[A](_p: =>Parsley[A], _op: =>Parsley[A => A])
    extends ChainLike[A](_p, _op, (l, r) => s"chainPre($r, $l)", new ChainPre(???, ???)) {
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
private [parsley] final class Chainl[A, B](_init: Parsley[B], _p: =>Parsley[A], _op: =>Parsley[(B, A) => B])
    extends Ternary[B, A, (B, A) => B, B](_init, _p, _op, (f, s, t) => s"chainl1($s, $t)", new Chainl(_, ???, ???)) {
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
private [parsley] final class Chainr[A, B](_p: =>Parsley[A], _op: =>Parsley[(A, B) => B], private [Chainr] val wrap: A => B)
    extends Binary[A, (A, B) => B, B](_p, _op, (l, r) => s"chainr1($l, $r)", new Chainr(???, ???, wrap)) {
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
private [parsley] final class SepEndBy1[A, B](_p: =>Parsley[A], _sep: =>Parsley[B])
    extends Binary[A, B, List[A]](_p, _sep, (l, r) => s"sepEndBy1($r, $l)", new SepEndBy1(???, ???)) {
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
private [parsley] final class ManyUntil[A](_body: Parsley[Any]) extends Unary[Any, List[A]](_body, c => s"manyUntil($c)", new ManyUntil(_)) {
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
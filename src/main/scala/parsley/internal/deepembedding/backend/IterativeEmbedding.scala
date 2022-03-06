package parsley.internal.deepembedding.backend

import parsley.internal.deepembedding.ContOps, ContOps.{result, ContAdapter}
import parsley.internal.deepembedding.singletons._
import parsley.internal.machine.instructions
import StrictParsley.InstrBuffer

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.higherKinds

private [backend] sealed abstract class ManyLike[A, B](name: String, unit: B, instr: Int => instructions.Instr) extends Unary[A, B] {
    final override def optimise: StrictParsley[B] = p match {
        case _: Pure[_] => throw new Exception(s"$name given parser which consumes no input")
        case _: MZero => new Pure(unit)
        case _ => this
    }
    final override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
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
private [deepembedding] final class Many[A](val p: StrictParsley[A]) extends ManyLike[A, List[A]]("many", Nil, new instructions.Many(_))
private [deepembedding] final class SkipMany[A](val p: StrictParsley[A]) extends ManyLike[A, Unit]("skipMany", (), new instructions.SkipMany(_))
private [backend] sealed abstract class ChainLike[A](p: StrictParsley[A], op: StrictParsley[A => A]) extends StrictParsley[A] {
    def inlinable = false
    override def optimise: StrictParsley[A] = op match {
        case _: Pure[_] => throw new Exception("chain given parser which consumes no input")
        case _: MZero => p
        case _ => this
    }
}
private [deepembedding] final class ChainPost[A](p: StrictParsley[A], op: StrictParsley[A => A]) extends ChainLike[A](p, op) {
    override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        val body = state.freshLabel()
        val handler = state.freshLabel()
        p.codeGen >> {
            instrs += new instructions.InputCheck(handler)
            instrs += new instructions.Label(body)
            op.codeGen |> {
                instrs += new instructions.Label(handler)
                instrs += new instructions.ChainPost(body)
            }
        }
    }
}
// This can't be fully strict, because it depends on binary!
private [deepembedding] final class ChainPre[A](p: StrictParsley[A], op: StrictParsley[A => A]) extends ChainLike[A](p, op) {
    override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        val body = state.freshLabel()
        val handler = state.freshLabel()
        instrs += new instructions.InputCheck(handler)
        instrs += new instructions.Label(body)
        op.codeGen >> {
            instrs += new instructions.Label(handler)
            instrs += new instructions.ChainPre(body)
            p.codeGen |>
            (instrs += instructions.Apply)
        }
    }
}
private [deepembedding] final class Chainl[A, B](init: StrictParsley[B], p: StrictParsley[A], op: StrictParsley[(B, A) => B]) extends StrictParsley[B] {
    def inlinable = false
    override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        val body = state.freshLabel()
        val handler = state.freshLabel()
        init.codeGen >> {
            instrs += new instructions.InputCheck(handler)
            instrs += new instructions.Label(body)
            op.codeGen[Cont, R] >>
            p.codeGen |> {
                instrs += new instructions.Label(handler)
                instrs += new instructions.Chainl(body)
            }
        }
    }
}
private [deepembedding] final class Chainr[A, B](p: StrictParsley[A], op: StrictParsley[(A, B) => B], private [Chainr] val wrap: A => B)
    extends StrictParsley[B] {
    def inlinable = false
    override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit]= {
        val body = state.freshLabel()
        val handler = state.freshLabel()
        instrs += new instructions.InputCheck(handler)
        instrs += new instructions.Label(body)
        p.codeGen >> {
            instrs += new instructions.InputCheck(handler)
            op.codeGen |> {
                instrs += new instructions.Label(handler)
                instrs += new instructions.Chainr(body, wrap)
            }
        }
    }
}
private [deepembedding] final class SepEndBy1[A, B](p: StrictParsley[A], sep: StrictParsley[B]) extends StrictParsley[List[A]] {
    def inlinable = false
    override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        val body = state.freshLabel()
        val handler = state.freshLabel()
        instrs += new instructions.InputCheck(handler)
        instrs += new instructions.Label(body)
        p.codeGen >> {
            instrs += new instructions.InputCheck(handler)
            sep.codeGen |> {
                instrs += new instructions.Label(handler)
                instrs += new instructions.SepEndBy1(body)
            }
        }
    }
}
private [deepembedding] final class ManyUntil[A](val p: StrictParsley[Any]) extends Unary[Any, List[A]] {
    override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
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
/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.backend

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.higherKinds

import parsley.internal.deepembedding.ContOps, ContOps.{ContAdapter, result, suspend}
import parsley.internal.deepembedding.singletons._
import parsley.internal.machine.instructions

import StrictParsley.InstrBuffer

private [backend] sealed abstract class ManyLike[A, B](name: String, unit: B) extends Unary[A, B] {
    def instr(label: Int): instructions.Instr
    def preamble(instrs: InstrBuffer): Unit
    final override def optimise: StrictParsley[B] = p match {
        case _: Pure[_] => throw new Exception(s"$name given parser which consumes no input") // scalastyle:ignore throw
        case _: MZero   => new Pure(unit)
        case _          => this
    }
    final override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        val body = state.freshLabel()
        val handler = state.freshLabel()
        preamble(instrs)
        instrs += new instructions.PushHandlerAndCheck(handler, saveHints = false)
        instrs += new instructions.Label(body)
        suspend(p.codeGen[Cont, R]) |> {
            instrs += new instructions.Label(handler)
            instrs += instr(body)
        }
    }
}
private [deepembedding] final class Many[A](val p: StrictParsley[A]) extends ManyLike[A, List[A]]("many", Nil) {
    override def instr(label: Int): instructions.Instr = new instructions.Many(label)
    override def preamble(instrs: InstrBuffer): Unit = instrs += new instructions.Fresh(mutable.ListBuffer.empty[Any])
    // $COVERAGE-OFF$
    final override def pretty(p: String): String = s"many($p)"
    // $COVERAGE-ON$
}
private [deepembedding] final class SkipMany[A](val p: StrictParsley[A]) extends ManyLike[A, Unit]("skipMany", ()) {
    override def instr(label: Int): instructions.Instr = new instructions.SkipMany(label)
    override def preamble(instrs: InstrBuffer): Unit = ()
    // $COVERAGE-OFF$
    final override def pretty(p: String): String = s"skipMany($p)"
    // $COVERAGE-ON$
}
private [backend] sealed abstract class ChainLike[A](p: StrictParsley[A], op: StrictParsley[A => A]) extends StrictParsley[A] {
    def inlinable: Boolean = false
    override def optimise: StrictParsley[A] = op match {
        case _: Pure[_] => throw new Exception("chain given parser which consumes no input") // scalastyle:ignore throw
        case _: MZero   => p
        case _          => this
    }
    // $COVERAGE-OFF$
    final override def pretty[Cont[_, +_]: ContOps, R]: Cont[R,String] = for (c1 <- p.pretty; c2 <- op.pretty) yield pretty(c1, c2)
    protected def pretty(p: String, op: String): String
    // $COVERAGE-ON$
}
private [deepembedding] final class ChainPost[A](p: StrictParsley[A], op: StrictParsley[A => A]) extends ChainLike[A](p, op) {
    override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        val body = state.freshLabel()
        val handler = state.freshLabel()
        suspend(p.codeGen[Cont, R]) >> {
            instrs += new instructions.PushHandlerAndCheck(handler, saveHints = false)
            instrs += new instructions.Label(body)
            suspend(op.codeGen[Cont, R]) |> {
                instrs += new instructions.Label(handler)
                instrs += new instructions.ChainPost(body)
            }
        }
    }
    // $COVERAGE-OFF$
    final override def pretty(p: String, op: String): String = s"chainPost($p, $op)"
    // $COVERAGE-ON$
}
private [deepembedding] final class ChainPre[A](p: StrictParsley[A], op: StrictParsley[A => A]) extends ChainLike[A](p, op) {
    override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        val body = state.freshLabel()
        val handler = state.freshLabel()
        instrs += new instructions.Push(identity[Any] _)
        instrs += new instructions.PushHandlerAndCheck(handler, saveHints = false)
        instrs += new instructions.Label(body)
        suspend(op.codeGen[Cont, R]) >> {
            instrs += new instructions.Label(handler)
            instrs += new instructions.ChainPre(body)
            suspend(p.codeGen[Cont, R]) |>
            (instrs += instructions.Apply)
        }
    }
    // $COVERAGE-OFF$
    final override def pretty(p: String, op: String): String = s"chainPre($op, $p)"
    // $COVERAGE-ON$
}
private [deepembedding] final class Chainl[A, B](init: StrictParsley[B], p: StrictParsley[A], op: StrictParsley[(B, A) => B]) extends StrictParsley[B] {
    def inlinable: Boolean = false
    override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        val body = state.freshLabel()
        val handler = state.freshLabel()
        suspend(init.codeGen[Cont, R]) >> {
            instrs += new instructions.PushHandlerAndCheck(handler, saveHints = false)
            instrs += new instructions.Label(body)
            op.codeGen[Cont, R] >>
            suspend(p.codeGen[Cont, R]) |> {
                instrs += new instructions.Label(handler)
                instrs += new instructions.Chainl(body)
            }
        }
    }
    // $COVERAGE-OFF$
    final override def pretty[Cont[_, +_]: ContOps, R]: Cont[R,String] =
        for {
            s1 <- init.pretty
            s2 <- p.pretty
            s3 <- op.pretty
        } yield s"chainl1($s1, $s2, $s3)"
    // $COVERAGE-ON$
}
private [deepembedding] final class Chainr[A, B](p: StrictParsley[A], op: StrictParsley[(A, B) => B], private [Chainr] val wrap: A => B)
    extends StrictParsley[B] {
    def inlinable: Boolean = false
    override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit]= {
        val body = state.freshLabel()
        val handler = state.freshLabel()
        instrs += new instructions.Push(identity[Any] _)
        instrs += new instructions.PushHandlerAndCheck(handler, saveHints = false)
        instrs += new instructions.Label(body)
        suspend(p.codeGen[Cont, R]) >> {
            instrs += new instructions.PushHandlerAndCheck(handler, saveHints = false)
            suspend(op.codeGen[Cont, R]) |> {
                instrs += new instructions.Label(handler)
                instrs += instructions.Chainr(body, wrap)
            }
        }
    }
    // $COVERAGE-OFF$
    final override def pretty[Cont[_, +_]: ContOps, R]: Cont[R,String] =
        for {
            s1 <- p.pretty
            s2 <- op.pretty
        } yield s"chainr1($s1, $s2)"
    // $COVERAGE-ON$
}
private [deepembedding] final class SepEndBy1[A, B](p: StrictParsley[A], sep: StrictParsley[B]) extends StrictParsley[List[A]] {
    def inlinable: Boolean = false
    override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        val body = state.freshLabel()
        val handler = state.freshLabel()
        instrs += new instructions.Fresh(mutable.ListBuffer.empty[Any])
        instrs += new instructions.PushHandlerAndCheck(handler, saveHints = false)
        instrs += new instructions.Label(body)
        suspend(p.codeGen[Cont, R]) >> {
            instrs += new instructions.PushHandlerAndCheck(handler, saveHints = false)
            suspend(sep.codeGen[Cont, R]) |> {
                instrs += new instructions.Label(handler)
                instrs += new instructions.SepEndBy1(body)
            }
        }
    }
    // $COVERAGE-OFF$
    final override def pretty[Cont[_, +_]: ContOps, R]: Cont[R,String] =
        for {
            s1 <- p.pretty
            s2 <- sep.pretty
        } yield s"sepEndBy1($s1, $s2)"
    // $COVERAGE-ON$
}
private [deepembedding] final class ManyUntil[A](val p: StrictParsley[Any]) extends Unary[Any, List[A]] {
    override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        val start = state.freshLabel()
        val loop = state.freshLabel()
        instrs += new instructions.Fresh(mutable.ListBuffer.empty[Any])
        instrs += new instructions.PushHandler(loop)
        instrs += new instructions.Label(start)
        suspend(p.codeGen[Cont, R]) |> {
            instrs += new instructions.Label(loop)
            instrs += new instructions.ManyUntil(start)
        }
    }
    // $COVERAGE-OFF$
    final override def pretty(p: String): String = s"manyUntil($p)"
    // $COVERAGE-ON$
}

/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.backend

import scala.collection.mutable

import parsley.exceptions.NonProductiveIterationException

import parsley.internal.deepembedding.ContOps, ContOps.{ContAdapter, suspend}
import parsley.internal.deepembedding.singletons._
import parsley.internal.machine.instructions

import StrictParsley.InstrBuffer

private [backend] sealed abstract class ManyLike[A, B](name: String, unit: B) extends Unary[A, B] {
    def genInstr(label: Int)(implicit instrs: InstrBuffer): Unit
    def genPostBody()(implicit instrs: InstrBuffer): Unit
    def preamble(instrs: InstrBuffer): Unit
    final override def optimise: StrictParsley[B] = p match {
        case _: Pure[_] => throw new NonProductiveIterationException(name) // scalastyle:ignore throw
        case _: MZero   => new Pure(unit)
        case _          => this
    }
    final override def codeGen[M[_, +_]: ContOps, R](producesResults: Boolean)(implicit instrs: InstrBuffer, state: CodeGenState): M[R, Unit] = {
        val body = state.freshLabel()
        val handler = state.freshLabel()
        preamble(instrs)
        instrs += new instructions.PushHandler(handler)
        instrs += new instructions.Label(body)
        suspend(p.codeGen[M, R](producesResults)) |> {
            genPostBody()
            instrs += new instructions.Label(handler)
            genInstr(body)
        }
    }
}
private [deepembedding] final class Many[A](val p: StrictParsley[A]) extends ManyLike[A, List[A]]("many", Nil) {
    override def genInstr(label: Int)(implicit instrs: InstrBuffer): Unit = instrs += new instructions.Many(label)
    override def genPostBody()(implicit instrs: InstrBuffer): Unit = ()
    override def preamble(instrs: InstrBuffer): Unit = instrs += new instructions.Fresh(mutable.ListBuffer.empty[Any])
    // $COVERAGE-OFF$
    final override def pretty(p: String): String = s"many($p)"
    // $COVERAGE-ON$
}
private [deepembedding] final class SkipMany[A](val p: StrictParsley[A]) extends ManyLike[A, Unit]("skipMany", ()) {
    override def genInstr(label: Int)(implicit instrs: InstrBuffer): Unit = {
        instrs += new instructions.SkipMany(label)
        instrs += instructions.Push.Unit
    }
    override def genPostBody()(implicit instrs: InstrBuffer): Unit = instrs += instructions.Pop
    override def preamble(instrs: InstrBuffer): Unit = ()
    // $COVERAGE-OFF$
    final override def pretty(p: String): String = s"skipMany($p)"
    // $COVERAGE-ON$
}
private [backend] sealed abstract class ChainLike[A](p: StrictParsley[A], op: StrictParsley[A => A]) extends StrictParsley[A] {
    def inlinable: Boolean = false
    override def optimise: StrictParsley[A] = op match {
        case _: Pure[_] => throw new NonProductiveIterationException("chain") // scalastyle:ignore throw
        case _: MZero   => p
        case _          => this
    }
    // $COVERAGE-OFF$
    final override def pretty: String = pretty(p.pretty, op.pretty)
    protected def pretty(p: String, op: String): String
    // $COVERAGE-ON$
}
private [deepembedding] final class ChainPost[A](p: StrictParsley[A], op: StrictParsley[A => A]) extends ChainLike[A](p, op) {
    override def codeGen[M[_, +_]: ContOps, R](producesResults: Boolean)(implicit instrs: InstrBuffer, state: CodeGenState): M[R, Unit] = {
        val body = state.freshLabel()
        val handler = state.freshLabel()
        suspend(p.codeGen[M, R](producesResults)) >> {
            instrs += new instructions.PushHandler(handler)
            instrs += new instructions.Label(body)
            suspend(op.codeGen[M, R](producesResults)) |> {
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
    override def codeGen[M[_, +_]: ContOps, R](producesResults: Boolean)(implicit instrs: InstrBuffer, state: CodeGenState): M[R, Unit] = {
        val body = state.freshLabel()
        val handler = state.freshLabel()
        instrs += new instructions.Push(identity[Any] _)
        instrs += new instructions.PushHandler(handler)
        instrs += new instructions.Label(body)
        suspend(op.codeGen[M, R](producesResults)) >> {
            instrs += new instructions.Label(handler)
            instrs += new instructions.ChainPre(body)
            suspend(p.codeGen[M, R](producesResults)) |>
            (instrs += instructions.Apply)
        }
    }
    // $COVERAGE-OFF$
    final override def pretty(p: String, op: String): String = s"chainPre($op, $p)"
    // $COVERAGE-ON$
}
private [deepembedding] final class Chainl[A, B](init: StrictParsley[B], p: StrictParsley[A], op: StrictParsley[(B, A) => B]) extends StrictParsley[B] {
    def inlinable: Boolean = false
    override def codeGen[M[_, +_]: ContOps, R](producesResults: Boolean)(implicit instrs: InstrBuffer, state: CodeGenState): M[R, Unit] = {
        val body = state.freshLabel()
        val handler = state.freshLabel()
        suspend(init.codeGen[M, R](producesResults)) >> {
            instrs += new instructions.PushHandler(handler)
            instrs += new instructions.Label(body)
            op.codeGen[M, R](producesResults) >>
            suspend(p.codeGen[M, R](producesResults)) |> {
                instrs += new instructions.Label(handler)
                instrs += new instructions.Chainl(body)
            }
        }
    }
    // $COVERAGE-OFF$
    final override def pretty: String = s"chainl1(${init.pretty}, ${p.pretty}, ${op.pretty})"
    // $COVERAGE-ON$
}
private [deepembedding] final class Chainr[A, B](p: StrictParsley[A], op: StrictParsley[(A, B) => B], private [Chainr] val wrap: A => B)
    extends StrictParsley[B] {
    def inlinable: Boolean = false
    override def codeGen[M[_, +_]: ContOps, R](producesResults: Boolean)(implicit instrs: InstrBuffer, state: CodeGenState): M[R, Unit]= {
        val body = state.freshLabel()
        // handler1 is where the check offset is kept
        val handler1 = state.getLabel(instructions.Refail)
        val handler2 = state.freshLabel()
        instrs += new instructions.Push(identity[Any] _)
        instrs += new instructions.PushHandler(handler1)
        instrs += new instructions.Label(body)
        suspend(p.codeGen[M, R](producesResults)) >> {
            instrs += new instructions.PushHandler(handler2)
            suspend(op.codeGen[M, R](producesResults)) |> {
                instrs += new instructions.ChainrJump(body)
                instrs += new instructions.Label(handler2)
                instrs += instructions.ChainrOpHandler(wrap)
            }
        }
    }
    // $COVERAGE-OFF$
    final override def pretty: String = s"chainr1(${p.pretty}, ${op.pretty})"
    // $COVERAGE-ON$
}
private [deepembedding] final class SepEndBy1[A, B](p: StrictParsley[A], sep: StrictParsley[B]) extends StrictParsley[List[A]] {
    def inlinable: Boolean = false
    override def codeGen[M[_, +_]: ContOps, R](producesResults: Boolean)(implicit instrs: InstrBuffer, state: CodeGenState): M[R, Unit] = {
        val body = state.freshLabel()
        val handler1 = state.freshLabel()
        val handler2 = state.freshLabel()
        instrs += new instructions.Fresh(mutable.ListBuffer.empty[Any])
        instrs += new instructions.PushHandler(handler1)
        instrs += new instructions.Label(body)
        suspend(p.codeGen[M, R](producesResults)) >> {
            instrs += new instructions.PushHandler(handler2)
            suspend(sep.codeGen[M, R](producesResults = false)) |> {
                instrs += instructions.Pop
                instrs += new instructions.SepEndBy1Jump(body)
                instrs += new instructions.Label(handler2)
                instrs += instructions.SepEndBy1SepHandler
                instrs += new instructions.Label(handler1)
                instrs += instructions.SepEndBy1WholeHandler
            }
        }
    }
    // $COVERAGE-OFF$
    final override def pretty: String = s"sepEndBy1(${p.pretty}, ${sep.pretty})"
    // $COVERAGE-ON$
}
private [deepembedding] final class ManyUntil[A](val p: StrictParsley[Any]) extends Unary[Any, List[A]] {
    override def codeGen[M[_, +_]: ContOps, R](producesResults: Boolean)(implicit instrs: InstrBuffer, state: CodeGenState): M[R, Unit] = {
        val start = state.freshLabel()
        val loop = state.freshLabel()
        instrs += new instructions.Fresh(mutable.ListBuffer.empty[Any])
        instrs += new instructions.PushHandler(loop)
        instrs += new instructions.Label(start)
        suspend(p.codeGen[M, R](producesResults)) |> {
            instrs += new instructions.Label(loop)
            instrs += new instructions.ManyUntil(start)
        }
    }
    // $COVERAGE-OFF$
    final override def pretty(p: String): String = s"manyUntil($p)"
    // $COVERAGE-ON$
}
private [deepembedding] final class SkipManyUntil(val p: StrictParsley[Any]) extends Unary[Any, Unit] {
    override def codeGen[M[_, +_]: ContOps, R](producesResults: Boolean)(implicit instrs: InstrBuffer, state: CodeGenState): M[R, Unit] = {
        val start = state.freshLabel()
        val loop = state.freshLabel()
        instrs += new instructions.PushHandler(loop)
        instrs += new instructions.Label(start)
        // requires the control flow through for the end token
        suspend(p.codeGen[M, R](producesResults = true)) |> {
            instrs += new instructions.Label(loop)
            instrs += new instructions.SkipManyUntil(start)
            instrs += instructions.Push.Unit
        }
    }
    // $COVERAGE-OFF$
    final override def pretty(p: String): String = s"manyUntil($p)"
    // $COVERAGE-ON$
}

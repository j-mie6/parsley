/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.backend

import parsley.internal.deepembedding.ContOps, ContOps.{suspend, ContAdapter}
import parsley.internal.deepembedding.singletons._
import parsley.internal.machine.instructions

import Branch.FlipApp
import StrictParsley.InstrBuffer

private [backend] sealed abstract class BranchLike[A, B, C, D](finaliser: Option[instructions.Instr]) extends StrictParsley[D] {
    val b: StrictParsley[A]
    val p: StrictParsley[B]
    val q: StrictParsley[C]
    def instr(label: Int): instructions.Instr

    def inlinable: Boolean = false
    final override def codeGen[M[_, +_]: ContOps, R](implicit instrs: InstrBuffer, state: CodeGenState): M[R, Unit] = {
        val toP = state.freshLabel()
        val end = state.freshLabel()
        suspend(b.codeGen[M, R]) >> {
            instrs += instr(toP)
            suspend(q.codeGen[M, R]) >> {
                for (instr <- finaliser) instrs += instr
                instrs += new instructions.Jump(end)
                instrs += new instructions.Label(toP)
                suspend(p.codeGen[M, R]) |> {
                    for (instr <- finaliser) instrs += instr
                    instrs += new instructions.Label(end)
                }
            }
        }
    }
}

private [deepembedding] final class Branch[A, B, C](val b: StrictParsley[Either[A, B]], val p: StrictParsley[A => C], val q: StrictParsley[B => C])
    extends BranchLike[Either[A, B], A => C, B => C, C](Some(FlipApp)) {
    override def instr(label: Int): instructions.Instr = new instructions.Case(label)
    override def optimise: StrictParsley[C] = b match {
        case Pure(Left(x)) => <*>(p, new Pure(x)).optimise
        case Pure(Right(y)) => <*>(q, new Pure(y)).optimise
        case _ => (p, q) match {
            case (Pure(f), Pure(g)) => <*>(new Pure((x: Either[A, B]) => x.fold(f, g)), b)
            case _ => this
        }
    }
    // $COVERAGE-OFF$
    final override def pretty: String = s"branch(${b.pretty}, ${p.pretty}, ${q.pretty})"
    // $COVERAGE-ON$
}

private [deepembedding] final class If[A](val b: StrictParsley[Boolean], val p: StrictParsley[A], val q: StrictParsley[A])
    extends BranchLike[Boolean, A, A, A](None) {
    override def instr(label: Int): instructions.Instr = new instructions.If(label)
    override def optimise: StrictParsley[A] = b match {
        case Pure(true) => p
        case Pure(false) => q
        case _ => this
    }
    // $COVERAGE-OFF$
    final override def pretty: String = s"if(${b.pretty}, ${p.pretty}, ${q.pretty})"
    // $COVERAGE-ON$
}

// Will need this again at some point...
private [backend] sealed abstract class FilterLike[A, B] extends StrictParsley[B] {
    protected val p: StrictParsley[A]
    protected val err: StrictParsley[((A, Int)) => Nothing]
    def inlinable: Boolean = false
    protected def instr(handler: Int, jumpLabel: Int): instructions.Instr

    final override def codeGen[M[_, +_]: ContOps, R](implicit instrs: InstrBuffer, state: CodeGenState): M[R, Unit] = {
        val handler1 = state.getLabel(instructions.PopStateAndFail)
        val handler2 = state.getLabel(instructions.AmendAndFail(false))
        val jumpLabel = state.freshLabel()
        instrs += new instructions.PushHandlerAndState(handler1, saveHints = false, hideHints = false)
        suspend(p.codeGen[M, R]) >> {
            instrs += instr(handler2, jumpLabel)
            suspend(err.codeGen[M, R]) |> {
                instrs += new instructions.Label(jumpLabel)
            }
        }
    }

    // $COVERAGE-OFF$
    final override def pretty: String = pretty(p.pretty, err.pretty)
    protected def pretty(p: String, err: String): String
    // $COVERAGE-ON$
}

private [deepembedding] final class Filter[A](val p: StrictParsley[A], pred: A => Boolean, val err: StrictParsley[((A, Int)) => Nothing])
    extends FilterLike[A, A] {
    final override def instr(handler: Int, jumpLabel: Int): instructions.Instr = new instructions.Filter(pred, jumpLabel, handler)
    final override def pretty(p: String, err: String): String = s"filterWith($p, ???, $err)"
}

private [deepembedding] final class MapFilter[A, B](val p: StrictParsley[A], pred: A => Option[B], val err: StrictParsley[((A, Int)) => Nothing])
    extends FilterLike[A, B] {
    final override def instr(handler: Int, jumpLabel: Int): instructions.Instr = new instructions.MapFilter(pred, jumpLabel, handler)
    final override def pretty(p: String, err: String): String = s"mapFilterWith($p, ???, $err)"
}

private [backend] object Branch {
    val FlipApp = instructions.Lift2[Any, Any => Any, Any]((x, f) => f(x))
}

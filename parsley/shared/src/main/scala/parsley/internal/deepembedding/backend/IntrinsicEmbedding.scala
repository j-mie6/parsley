/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.backend

import parsley.state.Ref

import parsley.internal.deepembedding.ContOps, ContOps.{suspend, ContAdapter}
import parsley.internal.machine.instructions

import StrictParsley.InstrBuffer

// TODO: Perform applicative fusion optimisations
private [deepembedding] final class Lift2[A, B, C](private val f: (A, B) => C, val left: StrictParsley[A], val right: StrictParsley[B])
    extends StrictParsley[C] {
    def inlinable: Boolean = false
    override def codeGen[M[_, +_]: ContOps, R](producesResults: Boolean)(implicit instrs: InstrBuffer, state: CodeGenState): M[R, Unit] = {
        suspend(left.codeGen[M, R](producesResults)) >>
        suspend(right.codeGen[M, R](producesResults)) |> {
            if (producesResults) instrs += instructions.Lift2(f)
        }
    }
    // $COVERAGE-OFF$
    final override def pretty: String = s"lift2(?, ${left.pretty}, ${right.pretty})"
    // $COVERAGE-ON$
}
private [deepembedding] final class Lift3[A, B, C, D](private val f: (A, B, C) => D, val p: StrictParsley[A], val q: StrictParsley[B], val r: StrictParsley[C])
    extends StrictParsley[D] {
    def inlinable: Boolean = false
    override def codeGen[M[_, +_]: ContOps, R](producesResults: Boolean)(implicit instrs: InstrBuffer, state: CodeGenState): M[R, Unit] = {
        suspend(p.codeGen[M, R](producesResults)) >>
        suspend(q.codeGen[M, R](producesResults)) >>
        suspend(r.codeGen[M, R](producesResults)) |> {
            if (producesResults) instrs += instructions.Lift3(f)
        }
    }
    // $COVERAGE-OFF$
    final override def pretty: String = s"lift3(?, ${p.pretty}, ${q.pretty}, ${r.pretty})"
    // $COVERAGE-ON$
}

private [deepembedding] final class Local[S, A](ref: Ref[S], left: StrictParsley[S], right: StrictParsley[A]) extends StrictParsley[A] {
    def inlinable: Boolean = false
    override def codeGen[M[_, +_]: ContOps, R](producesResults: Boolean)(implicit instrs: InstrBuffer, state: CodeGenState): M[R, Unit] = {
        suspend(left.codeGen[M, R](producesResults = true)) >> {
            instrs += new instructions.Get(ref.addr)
            instrs += new instructions.SwapAndPut(ref.addr)
            suspend(right.codeGen[M, R](producesResults)) |> {
                instrs += (if (producesResults) new instructions.SwapAndPut(ref.addr) else new instructions.Put(ref.addr))
            }
        }
    }
    // $COVERAGE-OFF$
    final override def pretty: String = s"local(r${ref.addr}, ${left.pretty}, ${right.pretty})"
    // $COVERAGE-ON$
}

private [backend] object Lift2 {
    def unapply[A, B, C](self: Lift2[A, B, C]): Some[((A, B) => C, StrictParsley[A], StrictParsley[B])] = Some((self.f, self.left, self.right))
}
private [backend] object Lift3 {
    def unapply[A, B, C, D](self: Lift3[A, B, C, D]): Some[((A, B, C) => D, StrictParsley[A], StrictParsley[B], StrictParsley[C])] = {
        Some((self.f, self.p, self.q, self.r))
    }
}

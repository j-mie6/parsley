/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.backend

import scala.language.higherKinds

import parsley.registers.Reg

import parsley.internal.deepembedding.ContOps, ContOps.{result, suspend, ContAdapter}
import parsley.internal.machine.instructions

import StrictParsley.InstrBuffer

// TODO: Perform applicative fusion optimisations
private [deepembedding] final class Lift2[A, B, C](private [Lift2] val f: (A, B) => C, val left: StrictParsley[A], val right: StrictParsley[B])
    extends StrictParsley[C] {
    def inlinable: Boolean = false
    override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont],  instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        suspend(left.codeGen[Cont, R]) >>
        suspend(right.codeGen[Cont, R]) |>
        (instrs += instructions.Lift2(f))
    }
    // $COVERAGE-OFF$
    final override def pretty[Cont[_, +_]: ContOps, R]: Cont[R,String] =
        for {
            s1 <- left.pretty
            s2 <- right.pretty
        } yield s"lift2(?, $s1, $s2)"
    // $COVERAGE-ON$
}
private [deepembedding] final class Lift3[A, B, C, D](val f: (A, B, C) => D, val p: StrictParsley[A], val q: StrictParsley[B], val r: StrictParsley[C])
    extends StrictParsley[D] {
    def inlinable: Boolean = false
    override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        suspend(p.codeGen[Cont, R]) >>
        suspend(q.codeGen[Cont, R]) >>
        suspend(r.codeGen[Cont, R]) |>
        (instrs += instructions.Lift3(f))
    }
    // $COVERAGE-OFF$
    final override def pretty[Cont[_, +_]: ContOps, R]: Cont[R,String] =
        for {
            s1 <- p.pretty
            s2 <- q.pretty
            s3 <- r.pretty
        } yield s"lift3(?, $s1, $s2, $s3)"
    // $COVERAGE-ON$
}

private [deepembedding] final class Local[S, A](reg: Reg[S], left: StrictParsley[S], right: StrictParsley[A]) extends StrictParsley[A] {
    def inlinable: Boolean = false
    override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        suspend(left.codeGen[Cont, R]) >> {
            instrs += new instructions.Get(reg.addr)
            instrs += new instructions.SwapAndPut(reg.addr)
            suspend(right.codeGen[Cont, R])|> {
                instrs += new instructions.SwapAndPut(reg.addr)
            }
        }
    }
    // $COVERAGE-OFF$
    final override def pretty[Cont[_, +_]: ContOps, R]: Cont[R,String] =
        for {
            s1 <- left.pretty
            s2 <- right.pretty
        } yield s"local(r${reg.addr}, $s1, $s2)"
    // $COVERAGE-ON$
}

private [backend] object Lift2 {
    def unapply[A, B, C](self: Lift2[A, B, C]): Option[((A, B) => C, StrictParsley[A], StrictParsley[B])] = Some((self.f, self.left, self.right))
}
private [backend] object Lift3 {
    def unapply[A, B, C, D](self: Lift3[A, B, C, D]): Option[((A, B, C) => D, StrictParsley[A], StrictParsley[B], StrictParsley[C])] = {
        Some((self.f, self.p, self.q, self.r))
    }
}

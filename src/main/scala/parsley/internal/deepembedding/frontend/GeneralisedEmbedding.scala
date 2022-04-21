/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.frontend

import scala.language.higherKinds

import parsley.internal.deepembedding.ContOps, ContOps.{result, suspend, ContAdapter}
import parsley.internal.deepembedding.backend, backend.StrictParsley

// Core Embedding
private [frontend] abstract class Unary[A, B](p: LazyParsley[A]) extends LazyParsley[B] {
    def pretty(p: String): String
    def make(p: StrictParsley[A]): StrictParsley[B]

    final override def findLetsAux[Cont[_, +_], R](seen: Set[LazyParsley[_]])(implicit ops: ContOps[Cont], state: LetFinderState): Cont[R,Unit] =
        suspend(p.findLets(seen))
    override def preprocess[Cont[_, +_], R, B_ >: B](implicit ops: ContOps[Cont], lets: LetMap, recs: RecMap): Cont[R, StrictParsley[B_]] =
        for (p <- suspend(p.optimised[Cont, R, A])) yield make(p)
    // $COVERAGE-OFF$
    final override def prettyASTAux[Cont[_, +_]](implicit ops: ContOps[Cont]): Cont[String, String] = for (c <- suspend(p.prettyASTAux)) yield pretty(c)
    // $COVERAGE-ON$
}

private [frontend] abstract class ScopedUnary[A, B](p: LazyParsley[A]) extends Unary[A, B](p) {
    def name: String
    final def pretty(c: String): String = s"$name($c)"
}

private [frontend] abstract class Binary[A, B, C](left: LazyParsley[A], _right: =>LazyParsley[B]) extends LazyParsley[C] {
    private lazy val right = _right

    def pretty(p: String, q: String): String
    def make(p: StrictParsley[A], q: StrictParsley[B]): StrictParsley[C]

    final override def findLetsAux[Cont[_, +_], R](seen: Set[LazyParsley[_]])(implicit ops: ContOps[Cont], state: LetFinderState): Cont[R,Unit] = {
        suspend(left.findLets[Cont, R](seen)) >> suspend(right.findLets(seen))
    }
    final override def preprocess[Cont[_, +_], R, C_ >: C](implicit ops: ContOps[Cont], lets: LetMap, recs: RecMap): Cont[R, StrictParsley[C_]] =
        for {
            left <- suspend(left.optimised[Cont, R, A])
            right <- suspend(right.optimised[Cont, R, B])
        } yield make(left, right)
    // $COVERAGE-OFF$
    final override def prettyASTAux[Cont[_, +_]](implicit ops: ContOps[Cont]): Cont[String, String] = {
        for {
            l <- suspend(left.prettyASTAux)
            r <- suspend(right.prettyASTAux)
        } yield pretty(l, r)
    }
    // $COVERAGE-ON$
}

private [frontend] abstract class Ternary[A, B, C, D](first: LazyParsley[A], _second: =>LazyParsley[B], _third: =>LazyParsley[C]) extends LazyParsley[D] {
    private lazy val second: LazyParsley[B] = _second
    private lazy val third: LazyParsley[C] = _third

    def pretty(p: String, q: String, r: String): String
    def make(p: StrictParsley[A], q: StrictParsley[B], r: StrictParsley[C]): StrictParsley[D]

    final override def findLetsAux[Cont[_, +_], R](seen: Set[LazyParsley[_]])(implicit ops: ContOps[Cont], state: LetFinderState): Cont[R, Unit] = {
        suspend(first.findLets[Cont, R](seen)) >> suspend(second.findLets(seen)) >> suspend(third.findLets(seen))
    }
    final override def preprocess[Cont[_, +_], R, D_ >: D](implicit ops: ContOps[Cont], lets: LetMap, recs: RecMap): Cont[R, StrictParsley[D_]] =
        for {
            first <- suspend(first.optimised[Cont, R, A])
            second <- suspend(second.optimised[Cont, R, B])
            third <- suspend(third.optimised[Cont, R, C])
        } yield make(first, second, third)
    // $COVERAGE-OFF$
    final override def prettyASTAux[Cont[_, +_]](implicit ops: ContOps[Cont]): Cont[String, String] =
        for {
            f <- suspend(first.prettyASTAux)
            s <- suspend(second.prettyASTAux)
            t <- suspend(third.prettyASTAux)
        } yield pretty(f, s, t)
    // $COVERAGE-ON$
}

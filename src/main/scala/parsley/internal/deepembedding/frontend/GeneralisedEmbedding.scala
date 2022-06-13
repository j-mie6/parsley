/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.frontend

import scala.language.higherKinds

import parsley.internal.deepembedding.ContOps, ContOps.{result, suspend, ContAdapter}
import parsley.internal.deepembedding.backend, backend.StrictParsley

// Core Embedding
private [frontend] abstract class Unary[A, B](p: LazyParsley[A]) extends LazyParsley[B] {
    def make(p: StrictParsley[A]): StrictParsley[B]

    final override def findLetsAux[Cont[_, +_]: ContOps, R](seen: Set[LazyParsley[_]])(implicit state: LetFinderState): Cont[R,Unit] =
        suspend(p.findLets(seen))
    override def preprocess[Cont[_, +_]: ContOps, R, B_ >: B](implicit lets: LetMap, recs: RecMap): Cont[R, StrictParsley[B_]] =
        for (p <- suspend(p.optimised[Cont, R, A])) yield make(p)
}

private [frontend] abstract class Binary[A, B, C](left: LazyParsley[A], _right: =>LazyParsley[B]) extends LazyParsley[C] {
    private lazy val right = _right

    def make(p: StrictParsley[A], q: StrictParsley[B]): StrictParsley[C]

    final override def findLetsAux[Cont[_, +_]: ContOps, R](seen: Set[LazyParsley[_]])(implicit state: LetFinderState): Cont[R,Unit] = {
        suspend(left.findLets[Cont, R](seen)) >> suspend(right.findLets(seen))
    }
    final override def preprocess[Cont[_, +_]: ContOps, R, C_ >: C](implicit lets: LetMap, recs: RecMap): Cont[R, StrictParsley[C_]] =
        for {
            left <- suspend(left.optimised[Cont, R, A])
            right <- suspend(right.optimised[Cont, R, B])
        } yield make(left, right)
}

private [frontend] abstract class Ternary[A, B, C, D](first: LazyParsley[A], _second: =>LazyParsley[B], _third: =>LazyParsley[C]) extends LazyParsley[D] {
    private lazy val second: LazyParsley[B] = _second
    private lazy val third: LazyParsley[C] = _third

    def make(p: StrictParsley[A], q: StrictParsley[B], r: StrictParsley[C]): StrictParsley[D]

    final override def findLetsAux[Cont[_, +_]: ContOps, R](seen: Set[LazyParsley[_]])(implicit state: LetFinderState): Cont[R, Unit] = {
        suspend(first.findLets[Cont, R](seen)) >> suspend(second.findLets(seen)) >> suspend(third.findLets(seen))
    }
    final override def preprocess[Cont[_, +_]: ContOps, R, D_ >: D](implicit lets: LetMap, recs: RecMap): Cont[R, StrictParsley[D_]] =
        for {
            first <- suspend(first.optimised[Cont, R, A])
            second <- suspend(second.optimised[Cont, R, B])
            third <- suspend(third.optimised[Cont, R, C])
        } yield make(first, second, third)
}

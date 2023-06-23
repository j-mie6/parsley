/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.frontend

import parsley.internal.deepembedding.ContOps, ContOps.{suspend, ContAdapter}
import parsley.internal.deepembedding.backend, backend.StrictParsley

// These all capture the general structures of combinators to factor out the common patterns for `findLetsAux` and `preprocess`.

private [frontend] abstract class Unary[A, B](p: LazyParsley[A]) extends LazyParsley[B] {
    def make(p: StrictParsley[A]): StrictParsley[B]

    final override def findLetsAux[M[_, _]: ContOps, R](seen: Set[LazyParsley[_]])(implicit state: LetFinderState): M[R,Unit] =
        suspend(p.findLets(seen))
    override def preprocess[M[_, _]: ContOps, R, B_ >: B](implicit lets: LetMap, recs: RecMap): M[R, StrictParsley[B_]] =
        for (p <- suspend(p.optimised[M, R, A])) yield make(p)
}

private [frontend] abstract class Binary[A, B, C](left: LazyParsley[A], _right: =>LazyParsley[B]) extends LazyParsley[C] {
    private lazy val right = _right

    def make(p: StrictParsley[A], q: StrictParsley[B]): StrictParsley[C]

    final override def findLetsAux[M[_, _]: ContOps, R](seen: Set[LazyParsley[_]])(implicit state: LetFinderState): M[R,Unit] = {
        suspend(left.findLets[M, R](seen)) >> suspend(right.findLets(seen))
    }
    final override def preprocess[M[_, _]: ContOps, R, C_ >: C](implicit lets: LetMap, recs: RecMap): M[R, StrictParsley[C_]] =
        for {
            left <- suspend(left.optimised[M, R, A])
            right <- suspend(right.optimised[M, R, B])
        } yield make(left, right)
}

private [frontend] abstract class Ternary[A, B, C, D](first: LazyParsley[A], _second: =>LazyParsley[B], _third: =>LazyParsley[C]) extends LazyParsley[D] {
    private lazy val second: LazyParsley[B] = _second
    private lazy val third: LazyParsley[C] = _third

    def make(p: StrictParsley[A], q: StrictParsley[B], r: StrictParsley[C]): StrictParsley[D]

    final override def findLetsAux[M[_, _]: ContOps, R](seen: Set[LazyParsley[_]])(implicit state: LetFinderState): M[R, Unit] = {
        suspend(first.findLets[M, R](seen)) >> suspend(second.findLets(seen)) >> suspend(third.findLets(seen))
    }
    final override def preprocess[M[_, _]: ContOps, R, D_ >: D](implicit lets: LetMap, recs: RecMap): M[R, StrictParsley[D_]] =
        for {
            first <- suspend(first.optimised[M, R, A])
            second <- suspend(second.optimised[M, R, B])
            third <- suspend(third.optimised[M, R, C])
        } yield make(first, second, third)
}

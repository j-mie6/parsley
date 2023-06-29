/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.frontend

import parsley.internal.deepembedding.ContOps, ContOps.{suspend, ContAdapter}
import parsley.internal.deepembedding.backend, backend.StrictParsley

private [parsley] final class <|>[A](p: LazyParsley[A], q: LazyParsley[A]) extends LazyParsley[A] {
    final override def findLetsAux[M[_, +_]: ContOps, R](seen: Set[LazyParsley[_]])(implicit state: LetFinderState): M[R,Unit] = {
        suspend(p.findLets[M, R](seen)) >> suspend(q.findLets(seen))
    }
    final override def preprocess[M[_, +_]: ContOps, R, A_ >: A](implicit lets: LetMap): M[R, StrictParsley[A_]] =
        for {
            p <- suspend(p.optimised[M, R, A])
            q <- suspend(q.optimised[M, R, A])
        } yield backend.<|>(p, q)

    final override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[A] = visitor.visit(this, context)(p, q)

    // XXX: Subject to change, due to the following:
    // - "<|>" is going out of style.
    // - "|" looks odd on its own.
    // - "orElse" is too wordy.
    // - "or" does not exist as a combinator.
    // We cannot think of any better options at the moment.
    override private [parsley] def prettyName = "<|>"
}

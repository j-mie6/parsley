/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.frontend

import parsley.internal.deepembedding.ContOps, ContOps.{suspend, ContAdapter}
import parsley.internal.deepembedding.backend, backend.StrictParsley

private [parsley] final class <|>[A](p: LazyParsley[A], q: LazyParsley[A]) extends LazyParsley[A] {
    final override def findLetsAux[Cont[_, +_]: ContOps, R](seen: Set[LazyParsley[_]])(implicit state: LetFinderState): Cont[R,Unit] = {
        suspend(p.findLets[Cont, R](seen)) >> suspend(q.findLets(seen))
    }
    final override def preprocess[Cont[_, +_]: ContOps, R, A_ >: A](implicit lets: LetMap, recs: RecMap): Cont[R, StrictParsley[A_]] =
        for {
            p <- suspend(p.optimised[Cont, R, A])
            q <- suspend(q.optimised[Cont, R, A])
        } yield backend.<|>(p, q)
}

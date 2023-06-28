/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.frontend.debugger

import parsley.internal.deepembedding.ContOps
import parsley.internal.deepembedding.ContOps.{ContAdapter, suspend}
import parsley.internal.deepembedding.backend.StrictParsley
import parsley.internal.deepembedding.frontend.{LazyParsley, LetFinderState, LetMap, RecMap}

// Wrapper parser class indicating explicitly named parsers
private [parsley] final class Named[A]
  (val par: LazyParsley[A], val name: String) extends LazyParsley[A] {
  def make(p: StrictParsley[A]): StrictParsley[A] = p

  override def findLetsAux[M[_, _] : ContOps, R](seen: Set[LazyParsley[_]])(implicit state: LetFinderState): M[R, Unit] =
    suspend(par.findLets(seen))

  override def preprocess[M[_, _] : ContOps, R, A_ >: A](implicit lets: LetMap, recs: RecMap): M[R, StrictParsley[A_]] =
    for (p <- suspend(par.optimised[M, R, A])) yield make(p)
}

private [parsley] object Named {
  def apply[A](par: LazyParsley[A], name: String): Named[A] =
    new Named(par, name)

  def unapply(p: LazyParsley[_]): Option[(LazyParsley[_], String)] =
    p match {
      case n: Named[_] => Some((n.par, n.name))
      case _           => None
    }
}

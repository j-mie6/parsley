/* SPDX-FileCopyrightText: © 2023 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.frontend.debugger

import parsley.debugger.internal.DebugContext
import parsley.internal.deepembedding.ContOps.{ContAdapter, suspend}
import parsley.internal.deepembedding.backend.StrictParsley
import parsley.internal.deepembedding.{backend, ContOps}
import parsley.internal.deepembedding.frontend.{LazyParsley, LetFinderState, LetMap, RecMap}

// Wrapper class signifying debugged classes
private [parsley] final class Debugged[A]
  (val origin: LazyParsley[A], var par: Option[LazyParsley[A]], val optName: Option[String])
  (implicit dbgCtx: DebugContext) extends LazyParsley[A] {
  def make(p: StrictParsley[A]): StrictParsley[A] =
    new backend.debugger.Debugged(origin, p, optName)

  override def findLetsAux[M[_, _] : ContOps, R](seen: Set[LazyParsley[_]])(implicit state: LetFinderState): M[R, Unit] =
    suspend(par.get.findLets(seen))

  override def preprocess[M[_, _] : ContOps, R, A_ >: A](implicit lets: LetMap, recs: RecMap): M[R, StrictParsley[A_]] =
    for (p <- suspend(par.get.optimised[M, R, A])) yield make(p)

  // Shortcuts to the given parser's name instead.
  def getTypeName: String = origin.getClass.getTypeName

  private [frontend] def withName(name: String): Debugged[A] =
    new Debugged(origin, par, Some(name))
}

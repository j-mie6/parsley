/* SPDX-FileCopyrightText: © 2023 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.frontend.debugger

import parsley.debugger.internal.DebugContext

import parsley.internal.deepembedding.{backend, ContOps}
import parsley.internal.deepembedding.ContOps.{suspend, ContAdapter}
import parsley.internal.deepembedding.backend.StrictParsley
import parsley.internal.deepembedding.frontend.{LazyParsley, LazyParsleyIVisitor, LetFinderState, LetMap, RecMap}

// Wrapper class signifying debugged classes
private [parsley] final class Debugged[A]
    (val origin: LazyParsley[A], var par: Option[LazyParsley[A]], val optName: Option[String])
    (dbgCtx: DebugContext) extends LazyParsley[A] {
    assert(!origin.isInstanceOf[Debugged[_]], "Debugged parsers should not be nested within each other directly.")

    def make(p: StrictParsley[A]): StrictParsley[A] =
        new backend.debugger.Debugged(origin, p, optName)(dbgCtx)

    override def findLetsAux[M[_, +_] : ContOps, R](seen: Set[LazyParsley[_]])(implicit state: LetFinderState): M[R, Unit] =
        suspend(par.get.findLets(seen))

    override def preprocess[M[_, +_] : ContOps, R, A_ >: A](implicit lets: LetMap, recs: RecMap): M[R, StrictParsley[A_]] =
        for (p <- suspend(par.get.optimised[M, R, A])) yield make(p)

    // Shortcuts to the given parser's name instead.
    def getTypeName: String = origin.getClass.getTypeName

    private [frontend] def withName(name: String): Debugged[A] =
        new Debugged(origin, par, Some(name))(dbgCtx)

    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[A] =
        visitor.visitUnknown(this, context)

    override private [parsley] def prettyName = optName.getOrElse(origin.prettyName)
}

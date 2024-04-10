/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.frontend.debugger

import parsley.XAssert
import parsley.debugger.internal.DebugContext

import parsley.internal.deepembedding.{backend, ContOps}
import parsley.internal.deepembedding.ContOps.{suspend, ContAdapter}
import parsley.internal.deepembedding.backend.StrictParsley
import parsley.internal.deepembedding.frontend.{LazyParsley, LazyParsleyIVisitor, LetFinderState, LetMap}

// Wrapper class signifying debugged classes
// TODO: the origin is needed to figure out the name later on... but couldn't we resolve the name here and avoid forwarding on to the backend (send string instead)?
// TODO: rename this to Tagged, which allows us to be generic over debugging strategies
private [parsley] final class Debugged[A](val origin: LazyParsley[A], var subParser: LazyParsley[A], val userAssignedName: Option[String])(dbgCtx: DebugContext)
    extends LazyParsley[A] {
    XAssert.assert(!origin.isInstanceOf[Debugged[_]], "Debugged parsers should not be nested within each other directly.")

    def make(p: StrictParsley[A]): StrictParsley[A] = new backend.debugger.Debugged(origin, p, userAssignedName)(dbgCtx)

    override def findLetsAux[M[_, +_] : ContOps, R](seen: Set[LazyParsley[_]])(implicit state: LetFinderState): M[R, Unit] = suspend(subParser.findLets(seen))
    override def preprocess[M[_, +_] : ContOps, R, A_ >: A](implicit lets: LetMap): M[R, StrictParsley[A_]] = {
        for (p <- suspend[M, R, StrictParsley[A]](subParser.optimised[M, R, A])) yield make(p)
    }

    // $COVERAGE-OFF$
    private [frontend] def withName(name: String): Debugged[A] = new Debugged(origin, subParser, Some(name))(dbgCtx)
    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[A] = visitor.visitUnknown(this, context)
    // $COVERAGE-ON$

    override private [parsley] def prettyName = userAssignedName.getOrElse(origin.prettyName)
}

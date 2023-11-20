/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.frontend.debugger

import parsley.internal.deepembedding.backend.StrictParsley
import parsley.internal.deepembedding.frontend.{LazyParsley, LazyParsleyIVisitor, Unary}

// $COVERAGE-OFF$
// Wrapper parser class indicating explicitly named parsers.
private [parsley] final class Named[A]
    (val par: LazyParsley[A], val name: String) extends Unary[A, A](par) {
    assert(!par.isInstanceOf[Named[_]], "Named parsers should not be nested within each other directly.")

    def make(p: StrictParsley[A]): StrictParsley[A] = p

    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[A] =
        visitor.visitUnknown(this, context)

    override private [parsley] def prettyName = name
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
// $COVERAGE-ON$

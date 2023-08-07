/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.frontend

import parsley.internal.deepembedding.backend, backend.StrictParsley

private [parsley] final class Branch[A, B, C](b: LazyParsley[Either[A, B]], p: =>LazyParsley[A => C], q: =>LazyParsley[B => C])
    extends Ternary[Either[A, B], A => C, B => C, C](b, p, q) {
    override def make(b: StrictParsley[Either[A, B]], p: StrictParsley[A => C], q: StrictParsley[B => C]): StrictParsley[C] = new backend.Branch(b, p, q)

    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[C] = visitor.visit(this, context)(b, p, q)
}

private [parsley] final class If[A](b: LazyParsley[Boolean], p: =>LazyParsley[A], q: =>LazyParsley[A]) extends Ternary[Boolean, A, A, A](b, p, q) {
    override def make(b: StrictParsley[Boolean], p: StrictParsley[A], q: StrictParsley[A]): StrictParsley[A] = new backend.If(b, p, q)

    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[A] = visitor.visit(this, context)(b, p, q)
}

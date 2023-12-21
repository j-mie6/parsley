/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.frontend

import parsley.registers.Reg

import parsley.internal.deepembedding.backend, backend.StrictParsley

private [parsley] final class Lift2[A, B, C](private val f: (A, B) => C, p: LazyParsley[A], q: =>LazyParsley[B]) extends Binary[A, B, C](p, q) {
    override def make(p: StrictParsley[A], q: StrictParsley[B]): StrictParsley[C] = new backend.Lift2(f, p, q)

    // $COVERAGE-OFF$
    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[C] = visitor.visit(this, context)(f, p, q)

    override private[parsley] def prettyName = "lift.lift2"
    // $COVERAGE-ON$
}
private [parsley] final class Lift3[A, B, C, D](private val f: (A, B, C) => D, p: LazyParsley[A], q: =>LazyParsley[B], r: =>LazyParsley[C])
    extends Ternary[A, B, C, D](p, q, r) {
    override def make(p: StrictParsley[A], q: StrictParsley[B], r: StrictParsley[C]): StrictParsley[D] = new backend.Lift3(f, p, q, r)

    // $COVERAGE-OFF$
    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[D] = visitor.visit(this, context)(f, p, q, r)

    override private[parsley] def prettyName = "lift.lift3"
    // $COVERAGE-ON$
}
private [parsley] final class Local[S, A](val reg: Reg[S], p: LazyParsley[S], q: =>LazyParsley[A]) extends Binary[S, A, A](p, q) with UsesRegister {
    override def make(p: StrictParsley[S], q: StrictParsley[A]): StrictParsley[A] = new backend.Local(reg, p, q)

    // $COVERAGE-OFF$
    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[A] = visitor.visit(this, context)(reg, p, q)

    override private[parsley] def prettyName = "local"
    // $COVERAGE-ON$
}

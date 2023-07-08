/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.frontend

import parsley.registers.Reg

import parsley.internal.deepembedding.backend, backend.StrictParsley

private [parsley] final class Lift2[A, B, C](private [Lift2] val f: (A, B) => C, p: LazyParsley[A], q: =>LazyParsley[B]) extends Binary[A, B, C](p, q) {
    override def make(p: StrictParsley[A], q: StrictParsley[B]): StrictParsley[C] = new backend.Lift2(f, p, q)

    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[C] = visitor.visit(this, context)(f, p, q)
}
private [parsley] final class Lift3[A, B, C, D](private [Lift3] val f: (A, B, C) => D, p: LazyParsley[A], q: =>LazyParsley[B], r: =>LazyParsley[C])
    extends Ternary[A, B, C, D](p, q, r) {
    override def make(p: StrictParsley[A], q: StrictParsley[B], r: StrictParsley[C]): StrictParsley[D] = new backend.Lift3(f, p, q, r)

    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[D] = visitor.visit(this, context)(f, p, q, r)
}
private [parsley] final class Local[S, A](val reg: Reg[S], p: LazyParsley[S], q: =>LazyParsley[A]) extends Binary[S, A, A](p, q) with UsesRegister {
    override def make(p: StrictParsley[S], q: StrictParsley[A]): StrictParsley[A] = new backend.Local(reg, p, q)

    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[A] = visitor.visit(this, context)(reg, p, q)
}

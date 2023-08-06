/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.frontend

import parsley.internal.deepembedding.backend, backend.StrictParsley

private [parsley] final class ErrorLabel[A](p: LazyParsley[A], labels: Seq[String]) extends Unary[A, A](p) {
    override def make(p: StrictParsley[A]): StrictParsley[A] = new backend.ErrorLabel(p, labels)

    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[A] = visitor.visit(this, context)(p, labels)
}
private [parsley] final class ErrorHide[A](p: LazyParsley[A]) extends Unary[A, A](p) {
    override def make(p: StrictParsley[A]): StrictParsley[A] = new backend.ErrorHide(p)

    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[A] = visitor.visit(this, context)(p)
}
private [parsley] final class ErrorExplain[A](p: LazyParsley[A], reason: String) extends Unary[A, A](p) {
    override def make(p: StrictParsley[A]): StrictParsley[A] = new backend.ErrorExplain(p, reason)

    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[A] = visitor.visit(this, context)(p, reason)
}

private [parsley] final class ErrorAmend[A](p: LazyParsley[A], partial: Boolean) extends Unary[A, A](p) {
    override def make(p: StrictParsley[A]): StrictParsley[A] = new backend.ErrorAmend(p, partial)

    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[A] = visitor.visit(this, context)(p, partial)
}
private [parsley] final class ErrorEntrench[A](p: LazyParsley[A]) extends Unary[A, A](p) {
    override def make(p: StrictParsley[A]): StrictParsley[A] = new backend.ErrorEntrench(p)

    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[A] = visitor.visit(this, context)(p)
}
private [parsley] final class ErrorDislodge[A](n: Int, p: LazyParsley[A]) extends Unary[A, A](p) {
    override def make(p: StrictParsley[A]): StrictParsley[A] = new backend.ErrorDislodge(n, p)

    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[A] = visitor.visit(this, context)(n, p)
}

private [parsley] final class ErrorLexical[A](p: LazyParsley[A]) extends Unary[A, A](p) {
    override def make(p: StrictParsley[A]): StrictParsley[A] = new backend.ErrorLexical(p)

    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[A] = visitor.visit(this, context)(p)
}

private [parsley] final class VerifiedError[A](p: LazyParsley[A], msggen: Either[A => Seq[String], Option[A => String]]) extends Unary[A, Nothing](p) {
    override def make(p: StrictParsley[A]): StrictParsley[Nothing] = new backend.VerifiedError(p, msggen)

    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[Nothing] = visitor.visit(this, context)(p, msggen)
}

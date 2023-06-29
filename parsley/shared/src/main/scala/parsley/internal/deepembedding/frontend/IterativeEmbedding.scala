/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.frontend

import parsley.internal.deepembedding.ContOps, ContOps.{suspend, ContAdapter}
import parsley.internal.deepembedding.backend, backend.StrictParsley

private [parsley] final class Many[A](p: LazyParsley[A]) extends Unary[A, List[A]](p) {
    override def make(p: StrictParsley[A]): StrictParsley[List[A]] = new backend.Many(p)

    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[List[A]] = visitor.visit(this, context)(p)

    override private [parsley] def prettyName = "many"
}
private [parsley] final class SkipMany[A](p: LazyParsley[A]) extends Unary[A, Unit](p) {
    override def make(p: StrictParsley[A]): StrictParsley[Unit] = new backend.SkipMany(p)

    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[Unit] = visitor.visit(this, context)(p)

    override private [parsley] def prettyName = "skipMany"
}
private [parsley] final class ChainPost[A](p: LazyParsley[A], _op: =>LazyParsley[A => A]) extends Binary[A, A => A, A](p, _op) {
    override def make(p: StrictParsley[A], op: StrictParsley[A => A]): StrictParsley[A] = new backend.ChainPost(p, op)

    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[A] = visitor.visit(this, context)(p, _op)

    override private [parsley] def prettyName = "chain.postfix"
}
private [parsley] final class ChainPre[A](p: LazyParsley[A], op: LazyParsley[A => A]) extends LazyParsley[A] {
    final override def findLetsAux[M[_, +_]: ContOps, R](seen: Set[LazyParsley[_]])(implicit state: LetFinderState): M[R, Unit] = {
        suspend(p.findLets[M, R](seen)) >> suspend(op.findLets(seen))
    }
    final override def preprocess[M[_, +_]: ContOps, R, A_ >: A](implicit lets: LetMap): M[R, StrictParsley[A_]] =
        for {
            p <- suspend(p.optimised[M, R, A])
            op <- suspend(op.optimised[M, R, A => A])
        } yield new backend.ChainPre(p, op)

    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[A] = visitor.visit(this, context)(p, op)

    override private [parsley] def prettyName = "chain.prefix"
}
private [parsley] final class Chainl[A, B](init: LazyParsley[B], p: =>LazyParsley[A], op: =>LazyParsley[(B, A) => B])
    extends Ternary[B, A, (B, A) => B, B](init, p, op) {
    override def make(init: StrictParsley[B], p: StrictParsley[A], op: StrictParsley[(B, A) => B]): StrictParsley[B] = new backend.Chainl(init, p, op)

    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[B] = visitor.visit(this, context)(init, p, op)

    override private [parsley] def prettyName = "infix.left1"
}
private [parsley] final class Chainr[A, B](p: LazyParsley[A], op: =>LazyParsley[(A, B) => B], private [Chainr] val wrap: A => B)
    extends Binary[A, (A, B) => B, B](p, op) {
    override def make(p: StrictParsley[A], op: StrictParsley[(A, B) => B]): StrictParsley[B] = new backend.Chainr(p, op, wrap)

    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[B] = visitor.visit(this, context)(p, op, wrap)

    override private [parsley] def prettyName = "infix.right1"
}
private [parsley] final class SepEndBy1[A, B](p: LazyParsley[A], sep: =>LazyParsley[B]) extends Binary[A, B, List[A]](p, sep) {
    override def make(p: StrictParsley[A], sep: StrictParsley[B]): StrictParsley[List[A]] = new backend.SepEndBy1(p, sep)

    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[List[A]] = visitor.visit(this, context)(p, sep)

    override private [parsley] def prettyName = "sepEndBy1"
}
private [parsley] final class ManyUntil[A](body: LazyParsley[Any]) extends Unary[Any, List[A]](body) {
    override def make(p: StrictParsley[Any]): StrictParsley[List[A]] = new backend.ManyUntil(p)

    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[List[A]] = visitor.visit(this, context)(body)

    override private [parsley] def prettyName = "manyUntil"
}
private [parsley] final class SkipManyUntil(body: LazyParsley[Any]) extends Unary[Any, Unit](body) {
    override def make(p: StrictParsley[Any]): StrictParsley[Unit] = new backend.SkipManyUntil(p)

    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[Unit] = visitor.visit(this, context)(body)

    override private [parsley] def prettyName = "skipManyUntil"
}

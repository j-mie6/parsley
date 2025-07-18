/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.frontend

import scala.collection.{mutable, Factory}

import parsley.internal.deepembedding.ContOps, ContOps.{suspend, ContAdapter}
import parsley.internal.deepembedding.backend, backend.StrictParsley

/**
  * This trait represents combinators that are iterative, that is, they execute 
  * parsers multiple times until they cannot match any more
  */
private [parsley] sealed trait Iterative

private [parsley] final class Many[A, C](init: LazyParsley[mutable.Builder[A, C]], p: =>LazyParsley[A], private [parsley] var debugName: String) 
    extends Binary[mutable.Builder[A, C], A, C](init, p) with Iterative {
    override def make(init: StrictParsley[mutable.Builder[A, C]], p: StrictParsley[A]): StrictParsley[C] = new backend.Many(init, p)

    // $COVERAGE-OFF$
    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[C] = visitor.visit(this, context)(init, p)
    // $COVERAGE-ON$
}
private [parsley] final class ChainPost[A](p: LazyParsley[A], _op: =>LazyParsley[A => A]) extends Binary[A, A => A, A](p, _op) with Iterative {
    override def make(p: StrictParsley[A], op: StrictParsley[A => A]): StrictParsley[A] = new backend.ChainPost(p, op)

    // $COVERAGE-OFF$
    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[A] = visitor.visit(this, context)(p, _op)

    private [parsley] var debugName = "chain.postfix"
    // $COVERAGE-ON$
}
private [parsley] final class ChainPre[A](p: LazyParsley[A], op: LazyParsley[A => A]) extends LazyParsley[A] with Iterative {
    final override def findLetsAux[M[_, +_]: ContOps, R](seen: Set[LazyParsley[_]])(implicit state: LetFinderState): M[R, Unit] = {
        suspend(p.findLets[M, R](seen)) >> suspend(op.findLets(seen))
    }
    final override def preprocess[M[_, +_]: ContOps, R, A_ >: A](implicit lets: LetMap): M[R, StrictParsley[A_]] =
        for {
            p <- suspend(p.optimised[M, R, A])
            op <- suspend(op.optimised[M, R, A => A])
        } yield new backend.ChainPre(p, op)

    // $COVERAGE-OFF$
    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[A] = visitor.visit(this, context)(p, op)

    private [parsley] var debugName = "chain.prefix"
    // $COVERAGE-ON$
}
private [parsley] final class Chainl[A, B](init: LazyParsley[B], p: =>LazyParsley[A], op: =>LazyParsley[(B, A) => B], var debugName: String)
    extends Ternary[B, A, (B, A) => B, B](init, p, op) with Iterative {
    override def make(init: StrictParsley[B], p: StrictParsley[A], op: StrictParsley[(B, A) => B]): StrictParsley[B] = new backend.Chainl(init, p, op)

    // $COVERAGE-OFF$
    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[B] = visitor.visit(this, context)(init, p, op)
    // $COVERAGE-ON$
}
private [parsley] final class Chainr[A, B](p: LazyParsley[A], op: =>LazyParsley[(A, B) => B], private [Chainr] val wrap: A => B)
    extends Binary[A, (A, B) => B, B](p, op) with Iterative {
    override def make(p: StrictParsley[A], op: StrictParsley[(A, B) => B]): StrictParsley[B] = new backend.Chainr(p, op, wrap)

    // $COVERAGE-OFF$
    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[B] = visitor.visit(this, context)(p, op, wrap)

    private [parsley] var debugName = "infix.right1"
    // $COVERAGE-ON$
}
private [parsley] final class SepEndBy1[A, C](p: LazyParsley[A], sep: =>LazyParsley[_], factory: Factory[A, C]) extends Binary[A, Any, C](p, sep) with Iterative {
    override def make(p: StrictParsley[A], sep: StrictParsley[Any]): StrictParsley[C] = new backend.SepEndBy1(p, sep, factory)

    // $COVERAGE-OFF$
    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[C] = visitor.visit(this, context)(p, sep, factory)

    private [parsley] var debugName = "sepEndBy1"
    // $COVERAGE-ON$
}
private [parsley] final class ManyTill[A, C](init: LazyParsley[mutable.Builder[A, C]], body: =>LazyParsley[Any], private [parsley] var debugName: String)
    extends Binary[mutable.Builder[A, C], Any, C](init, body) with Iterative {
    override def make(init: StrictParsley[mutable.Builder[A, C]], p: StrictParsley[Any]): StrictParsley[C] = new backend.ManyUntil(init, p)

    // $COVERAGE-OFF$
    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[C] = visitor.visit(this, context)(init, body)
    // $COVERAGE-ON$
}
private [parsley] final class SkipManyUntil(body: LazyParsley[Any]) extends Unary[Any, Unit](body) with Iterative {
    override def make(p: StrictParsley[Any]): StrictParsley[Unit] = new backend.SkipManyUntil(p)

    // $COVERAGE-OFF$
    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[Unit] = visitor.visit(this, context)(body)

    private [parsley] var debugName = "skipManyTill"
    // $COVERAGE-ON$
}

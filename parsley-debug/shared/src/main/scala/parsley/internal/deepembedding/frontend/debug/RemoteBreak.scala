/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.frontend.debug

import parsley.state.Ref
import parsley.internal.deepembedding.backend.StrictParsley
import parsley.internal.deepembedding.frontend.{LazyParsley, LazyParsleyIVisitor, Unary}
import parsley.debug.Breakpoint

private [parsley] final class RemoteBreak[A](p: LazyParsley[A], val break: Breakpoint) extends Unary[A, A](p) {
    override def make(p: StrictParsley[A]): StrictParsley[A] = p

    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[A] = visitor.visitGeneric(this, context)

    private [parsley] var debugName: String = "remoteBreak"
}

private [parsley] final class RemoteManageableBreak[A](p: LazyParsley[A], val break: Breakpoint, val refs: Ref[Any]*) extends Unary[A, A](p) {
    override def make(p: StrictParsley[A]): StrictParsley[A] = p

    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[A] = visitor.visitGeneric(this, context)

    private [parsley] var debugName: String = "remoteManageableBreak"
}

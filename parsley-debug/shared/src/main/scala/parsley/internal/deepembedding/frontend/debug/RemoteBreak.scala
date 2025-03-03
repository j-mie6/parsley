/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.frontend.debug

import parsley.debug.Breakpoint
import parsley.internal.deepembedding.backend
import parsley.internal.deepembedding.backend.StrictParsley
import parsley.internal.deepembedding.frontend.{LazyParsley, LazyParsleyIVisitor, Unary}

private [parsley] final class RemoteBreak[A](p: LazyParsley[A], break: Breakpoint) extends Unary[A, A](p) {
    override def make(p: StrictParsley[A]): StrictParsley[A] = new backend.InertBreak(p, break)

    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[A] = visitor.visitGeneric(this, context)

    private [parsley] var debugName: String = "remoteBreak"
}

/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.frontend.debug

import parsley.debug.Breakpoint
import parsley.internal.deepembedding.backend.{StrictParsley, InertBreak}
import parsley.internal.deepembedding.frontend.{LazyParsley, LazyParsleyIVisitor, Unary}
import parsley.internal.machine.RefCodec

private [parsley] final class RemoteBreak[A](p: LazyParsley[A], break: Breakpoint, refs: RefCodec*) extends Unary[A, A](p) {
    override def make(p: StrictParsley[A]): StrictParsley[A] = new InertBreak(p, break, refs*)

    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[A] = visitor.visitGeneric(this, context)

    private [parsley] var debugName: String = "remoteBreak"
}

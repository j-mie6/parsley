package parsley.internal.deepembedding.frontend.debug

import parsley.internal.deepembedding.backend.StrictParsley
import parsley.internal.deepembedding.frontend.{LazyParsley, LazyParsleyIVisitor, Unary}
import parsley.debug.Breakpoint

private [parsley] final class RemoteBreak[A](p: LazyParsley[A], val break: Breakpoint) extends Unary[A, A](p) {
    override def make(p: StrictParsley[A]): StrictParsley[A] = p

    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[A] = p.visit(visitor, context)

    private [parsley] var debugName: String = "dillBreak"
}

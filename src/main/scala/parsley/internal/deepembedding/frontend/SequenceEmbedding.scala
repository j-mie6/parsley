package parsley.internal.deepembedding.frontend

import parsley.internal.machine.instructions

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.higherKinds
import parsley.internal.deepembedding.backend, backend.StrictParsley

// Core Embedding
private [parsley] final class <*>[A, B](pf: LazyParsley[A => B], px: =>LazyParsley[A]) extends Binary[A => B, A, B](pf, px) {
    // $COVERAGE-OFF$
    override def pretty(l: String, r: String): String = s"($l <*> $r)"
    // $COVERAGE-ON$
    override def make(pf: StrictParsley[A => B], px: StrictParsley[A]): StrictParsley[B] = new backend.<*>(pf, px)
}

private [parsley] final class >>=[A, B](p: LazyParsley[A], private [>>=] val f: A => LazyParsley[B]) extends Unary[A, B](p) {
    // $COVERAGE-OFF$
    override def pretty(l: String): String = s"($l >>= ?)"
    // $COVERAGE-ON$
    override def make(p: StrictParsley[A]): StrictParsley[B] = new backend.>>=(p, f)
}

private [parsley] final class *>[A](_p: LazyParsley[_], _q: =>LazyParsley[A]) extends Binary[Any, A, A](_p, _q) {
    // $COVERAGE-OFF$
    override def pretty(l: String, r: String): String = s"($l *> $r)"
    // $COVERAGE-ON$
    override def make(p: StrictParsley[Any], q: StrictParsley[A]): StrictParsley[A] = new backend.*>(p, q)
}
private [parsley] final class <*[A](_p: LazyParsley[A], _q: =>LazyParsley[_]) extends Binary[A, Any, A](_p, _q) {
    // $COVERAGE-OFF$
    override def pretty(l: String, r: String): String = s"($l <* $r)"
    // $COVERAGE-ON$
    override def make(p: StrictParsley[A], q: StrictParsley[Any]): StrictParsley[A] = new backend.<*(p, q)
}
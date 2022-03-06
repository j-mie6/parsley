package parsley.internal.deepembedding.frontend

import parsley.internal.deepembedding.ContOps.{result, ContAdapter}
import parsley.internal.machine.instructions

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.higherKinds
import parsley.internal.deepembedding.backend, backend.StrictParsley

// Core Embedding
private [parsley] final class <*>[A, B](pf: LazyParsley[A => B], px: =>LazyParsley[A]) extends Binary[A => B, A, B](pf, px) {
    override def pretty(l: String, r: String) = s"($l <*> $r)"
    override def make(pf: StrictParsley[A => B], px: StrictParsley[A]) = new backend.<*>(pf, px)
}

private [parsley] final class >>=[A, B](p: LazyParsley[A], private [>>=] val f: A => LazyParsley[B]) extends Unary[A, B](p) {
    override def pretty(l: String) = s"($l >>= ?)"
    override def make(p: StrictParsley[A]) = new backend.>>=(p, f)
}

private [parsley] final class *>[A](_p: LazyParsley[_], _q: =>LazyParsley[A]) extends Binary[Any, A, A](_p, _q) {
    override def pretty(l: String, r: String) = s"($l *> $r)"
    override def make(p: StrictParsley[Any], q: StrictParsley[A]) = new backend.*>(p, q)
}
private [parsley] final class <*[A](_p: LazyParsley[A], _q: =>LazyParsley[_]) extends Binary[A, Any, A](_p, _q) {
    override def pretty(l: String, r: String) = s"($l <* $r)"
    override def make(p: StrictParsley[A], q: StrictParsley[Any]) = new backend.<*(p, q)
}
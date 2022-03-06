package parsley.internal.deepembedding.frontend

import parsley.internal.deepembedding.ContOps.{result, ContAdapter}
import parsley.internal.machine.instructions

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.higherKinds
import parsley.internal.deepembedding.backend, backend.StrictParsley

// Core Embedding
private [parsley] final class <*>[A, B](pf: LazyParsley[A => B], px: =>LazyParsley[A]) extends Binary[A => B, A, B](pf, px, new backend.<*>(_, _)) {
    override def pretty(l: String, r: String) = s"($l <*> $r)"
}

private [parsley] final class >>=[A, B](p: LazyParsley[A], private [>>=] val f: A => LazyParsley[B]) extends Unary[A, B](p, new backend.>>=(_, f)) {
    override def pretty(l: String) = s"($l >>= ?)"
}

private [parsley] final class *>[A](_p: LazyParsley[_], _q: =>LazyParsley[A]) extends Binary[Any, A, A](_p, _q, new backend.*>(_, _)) {
    override def pretty(l: String, r: String) = s"($l *> $r)"
}
private [parsley] final class <*[A](_p: LazyParsley[A], _q: =>LazyParsley[_]) extends Binary[A, Any, A](_p, _q, new backend.<*(_, _)) {
    override def pretty(l: String, r: String) = s"($l <* $r)"
}
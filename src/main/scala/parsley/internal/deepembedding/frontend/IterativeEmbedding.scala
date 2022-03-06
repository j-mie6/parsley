package parsley.internal.deepembedding.frontend

import parsley.internal.deepembedding.ContOps, ContOps.{result, ContAdapter}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.higherKinds

import parsley.internal.deepembedding.backend

private [parsley] final class Many[A](p: LazyParsley[A]) extends ScopedUnary[A, List[A]](p, new backend.Many(_)) {
    override val name = "many"
}
private [parsley] final class SkipMany[A](p: LazyParsley[A]) extends ScopedUnary[A, Unit](p, new backend.SkipMany(_)) {
    override val name = "skipMany"
}
private [parsley] final class ChainPost[A](p: LazyParsley[A], _op: =>LazyParsley[A => A]) extends Binary[A, A => A, A](p, _op, new backend.ChainPost(_, _)) {
    override def pretty(l: String, r: String) = s"chainPost($l, $r)"
}

// This can't be fully strict, because it depends on binary!
private [parsley] final class ChainPre[A](p: LazyParsley[A], op: =>LazyParsley[A => A]) extends Binary[A, A => A, A](p, op, new backend.ChainPre(_, _)) {
    override def pretty(l: String, r: String) = s"chainPre($r, $l)"
}
private [parsley] final class Chainl[A, B](init: LazyParsley[B], p: =>LazyParsley[A], op: =>LazyParsley[(B, A) => B]) extends Ternary[B, A, (B, A) => B, B](init, p, op, new backend.Chainl(_, _, _)) {
    override def pretty(f: String, s: String, t: String) = s"chainl1($s, $t)"
}
private [parsley] final class Chainr[A, B](p: LazyParsley[A], op: =>LazyParsley[(A, B) => B], private [Chainr] val wrap: A => B) extends Binary[A, (A, B) => B, B](p, op, new backend.Chainr(_, _, wrap))  {
    override def pretty(l: String, r: String) = s"chainl1($l, $r)"
}
private [parsley] final class SepEndBy1[A, B](p: LazyParsley[A], sep: =>LazyParsley[B]) extends Binary[A, B, List[A]](p, sep, new backend.SepEndBy1(_, _)) {
    override def pretty(l: String, r: String) = s"sepEndBy1($r, $l)"
}
private [parsley] final class ManyUntil[A](body: LazyParsley[Any]) extends Unary[Any, List[A]](body, new backend.ManyUntil(_)) {
    override def pretty(c: String) = s"manyUntil($c)"
}
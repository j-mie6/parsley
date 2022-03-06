package parsley.internal.deepembedding.frontend

import parsley.internal.deepembedding.ContOps.{result, ContAdapter}

import parsley.internal.deepembedding.backend, backend.StrictParsley

import scala.language.higherKinds

private [parsley] final class Branch[A, B, C](b: LazyParsley[Either[A, B]], p: =>LazyParsley[A => C], q: =>LazyParsley[B => C])
    extends Ternary[Either[A, B], A => C, B => C, C](b, p, q) {
    override def pretty(f: String, s: String, t: String) = s"branch($f, $s, $t)"
    override def make(b: StrictParsley[Either[A, B]], p: StrictParsley[A => C], q: StrictParsley[B => C]) = new backend.Branch(b, p, q)
}

private [parsley] final class If[A](b: LazyParsley[Boolean], p: =>LazyParsley[A], q: =>LazyParsley[A]) extends Ternary[Boolean, A, A, A](b, p, q) {
    override def pretty(f: String, s: String, t: String) = s"($f ? $s : $t)"
    override def make(b: StrictParsley[Boolean], p: StrictParsley[A], q: StrictParsley[A]) = new backend.If(b, p, q)
}

private [parsley] final class FastFail[A](p: LazyParsley[A], msggen: A => String) extends Unary[A, Nothing](p) {
    override def pretty(c: String) = s"$c ! ?"
    override def make(p: StrictParsley[A]) = new backend.FastFail(p, msggen)
}
private [parsley] final class FastUnexpected[A](p: LazyParsley[A], msggen: A => String) extends Unary[A, Nothing](p) {
    override def pretty(c: String) = s"$c.unexpected(?)"
    override def make(p: StrictParsley[A]) = new backend.FastUnexpected(p, msggen)
}

private [parsley] final class Filter[A](p: LazyParsley[A], pred: A => Boolean) extends Unary[A, A](p) {
    override def pretty(c: String) = s"$c.filter(?)"
    override def make(p: StrictParsley[A]) = new backend.Filter(p, pred)
}
private [parsley] final class FilterOut[A](p: LazyParsley[A], pred: PartialFunction[A, String]) extends Unary[A, A](p) {
    override def pretty(c: String) = s"$c.filterOut(?)"
    override def make(p: StrictParsley[A]) = new backend.FilterOut(p, pred)
}
private [parsley] final class GuardAgainst[A](p: LazyParsley[A], pred: PartialFunction[A, String]) extends Unary[A, A](p) {
    override def pretty(c: String) = s"$c.guardAgainst(?)"
    override def make(p: StrictParsley[A]) = new backend.GuardAgainst(p, pred)
}
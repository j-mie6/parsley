package parsley.internal.deepembedding.frontend

import parsley.internal.deepembedding.ContOps.{result, ContAdapter}

import parsley.internal.deepembedding.backend

import scala.language.higherKinds

private [parsley] final class Branch[A, B, C](b: LazyParsley[Either[A, B]], p: =>LazyParsley[A => C], q: =>LazyParsley[B => C])
    extends Ternary[Either[A, B], A => C, B => C, C](b, p, q, new backend.Branch(_, _, _)) {
    override def pretty(f: String, s: String, t: String) = s"branch($f, $s, $t)"
}

private [parsley] final class If[A](b: LazyParsley[Boolean], p: =>LazyParsley[A], q: =>LazyParsley[A])
    extends Ternary[Boolean, A, A, A](b, p, q, new backend.If(_, _, _)) {
    override def pretty(f: String, s: String, t: String) = s"($f ? $s : $t)"
}

private [parsley] final class FastFail[A](p: LazyParsley[A], msggen: A => String) extends Unary[A, Nothing](p, new backend.FastFail(_, msggen)) {
    override def pretty(c: String) = s"$c ! ?"
}
private [parsley] final class FastUnexpected[A](p: LazyParsley[A], msggen: A => String) extends Unary[A, Nothing](p, new backend.FastUnexpected(_, msggen)) {
    override def pretty(c: String) = s"$c.unexpected(?)"
}

private [parsley] final class Filter[A](p: LazyParsley[A], pred: A => Boolean) extends Unary[A, A](p, new backend.Filter(_, pred)) {
    override def pretty(c: String) = s"$c.filter(?)"
}
private [parsley] final class FilterOut[A](p: LazyParsley[A], pred: PartialFunction[A, String]) extends Unary[A, A](p, new backend.FilterOut(_, pred)) {
    override def pretty(c: String) = s"$c.filterOut(?)"
}
private [parsley] final class GuardAgainst[A](p: LazyParsley[A], pred: PartialFunction[A, String]) extends Unary[A, A](p, new backend.GuardAgainst(_, pred)) {
    override def pretty(c: String) = s"$c.guardAgainst(?)"
}
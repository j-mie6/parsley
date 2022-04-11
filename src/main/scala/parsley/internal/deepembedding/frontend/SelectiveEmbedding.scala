package parsley.internal.deepembedding.frontend

import parsley.internal.deepembedding.backend, backend.StrictParsley

import scala.language.higherKinds

private [parsley] final class Branch[A, B, C](b: LazyParsley[Either[A, B]], p: =>LazyParsley[A => C], q: =>LazyParsley[B => C])
    extends Ternary[Either[A, B], A => C, B => C, C](b, p, q) {
    // $COVERAGE-OFF$
    override def pretty(f: String, s: String, t: String): String = s"branch($f, $s, $t)"
    // $COVERAGE-ON$
    override def make(b: StrictParsley[Either[A, B]], p: StrictParsley[A => C], q: StrictParsley[B => C]): StrictParsley[C] = new backend.Branch(b, p, q)
}

private [parsley] final class If[A](b: LazyParsley[Boolean], p: =>LazyParsley[A], q: =>LazyParsley[A]) extends Ternary[Boolean, A, A, A](b, p, q) {
    // $COVERAGE-OFF$
    override def pretty(f: String, s: String, t: String): String = s"($f ? $s : $t)"
    // $COVERAGE-ON$
    override def make(b: StrictParsley[Boolean], p: StrictParsley[A], q: StrictParsley[A]): StrictParsley[A] = new backend.If(b, p, q)
}

private [parsley] final class FastFail[A](p: LazyParsley[A], msggen: A => String) extends Unary[A, Nothing](p) {
    // $COVERAGE-OFF$
    override def pretty(c: String): String = s"$c ! ?"
    // $COVERAGE-ON$
    override def make(p: StrictParsley[A]): StrictParsley[Nothing] = new backend.FastFail(p, msggen)
}
private [parsley] final class FastUnexpected[A](p: LazyParsley[A], msggen: A => String) extends Unary[A, Nothing](p) {
    // $COVERAGE-OFF$
    override def pretty(c: String): String = s"$c.unexpected(?)"
    // $COVERAGE-ON$
    override def make(p: StrictParsley[A]): StrictParsley[Nothing] = new backend.FastUnexpected(p, msggen)
}

private [parsley] final class Filter[A](p: LazyParsley[A], pred: A => Boolean) extends Unary[A, A](p) {
    // $COVERAGE-OFF$
    override def pretty(c: String): String = s"$c.filter(?)"
    // $COVERAGE-ON$
    override def make(p: StrictParsley[A]): StrictParsley[A] = new backend.Filter(p, pred)
}
private [parsley] final class FilterOut[A](p: LazyParsley[A], pred: PartialFunction[A, String]) extends Unary[A, A](p) {
    // $COVERAGE-OFF$
    override def pretty(c: String): String = s"$c.filterOut(?)"
    // $COVERAGE-ON$
    override def make(p: StrictParsley[A]): StrictParsley[A] = new backend.FilterOut(p, pred)
}
private [parsley] final class GuardAgainst[A](p: LazyParsley[A], pred: PartialFunction[A, String]) extends Unary[A, A](p) {
    // $COVERAGE-OFF$
    override def pretty(c: String): String = s"$c.guardAgainst(?)"
    // $COVERAGE-ON$
    override def make(p: StrictParsley[A]): StrictParsley[A] = new backend.GuardAgainst(p, pred)
}
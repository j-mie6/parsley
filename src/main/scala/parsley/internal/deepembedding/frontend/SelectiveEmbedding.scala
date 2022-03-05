package parsley.internal.deepembedding.frontend

import parsley.internal.deepembedding.ContOps.{result, ContAdapter}

import parsley.internal.deepembedding.backend

import scala.language.higherKinds

private [parsley] final class Branch[A, B, C](b: LazyParsley[Either[A, B]], p: =>LazyParsley[A => C], q: =>LazyParsley[B => C])
    extends Ternary[Either[A, B], A => C, B => C, C](b, p, q, (f, s, t) => s"branch($f, $s, $t)", new backend.Branch(_, _, _))

private [parsley] final class If[A](b: LazyParsley[Boolean], p: =>LazyParsley[A], q: =>LazyParsley[A])
    extends Ternary[Boolean, A, A, A](b, p, q, (f, s, t) => s"($f ? $s : $t)", new backend.If(_, _, _))

private [parsley] final class FastFail[A](p: LazyParsley[A], msggen: A => String)
    extends Unary[A, Nothing](p, c => s"$c ! ?", new backend.FastFail(_, msggen))
private [parsley] final class FastUnexpected[A](p: LazyParsley[A], msggen: A => String)
    extends Unary[A, Nothing](p, c => s"$c.unexpected(?)", new backend.FastUnexpected(_, msggen))

private [parsley] final class Filter[A](p: LazyParsley[A], pred: A => Boolean) extends Unary[A, A](p, c => s"$c.filter(?)", new backend.Filter(_, pred))
private [parsley] final class FilterOut[A](p: LazyParsley[A], pred: PartialFunction[A, String])
    extends Unary[A, A](p, c => s"$c.filterOut(?)", new backend.FilterOut(_, pred))
private [parsley] final class GuardAgainst[A](p: LazyParsley[A], pred: PartialFunction[A, String])
    extends Unary[A, A](p, c => s"$c.guardAgainst(?)", new backend.GuardAgainst(_, pred))
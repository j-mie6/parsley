package parsley.internal.deepembedding

import ContOps.{result, ContAdapter}

import scala.language.higherKinds

private [parsley] final class Branch[A, B, C](b: Parsley[Either[A, B]], p: =>Parsley[A => C], q: =>Parsley[B => C])
    extends Ternary[Either[A, B], A => C, B => C, C](b, p, q, (f, s, t) => s"branch($f, $s, $t)", new backend.Branch(_, _, _))

private [parsley] final class If[A](b: Parsley[Boolean], p: =>Parsley[A], q: =>Parsley[A])
    extends Ternary[Boolean, A, A, A](b, p, q, (f, s, t) => s"($f ? $s : $t)", new backend.If(_, _, _))

private [parsley] final class FastFail[A](p: Parsley[A], msggen: A => String)
    extends Unary[A, Nothing](p, c => s"$c ! ?", new backend.FastFail(_, msggen)) with MZero
private [parsley] final class FastUnexpected[A](p: Parsley[A], msggen: A => String)
    extends Unary[A, Nothing](p, c => s"$c.unexpected(?)", new backend.FastUnexpected(_, msggen)) with MZero

private [parsley] final class Filter[A](p: Parsley[A], pred: A => Boolean) extends Unary[A, A](p, c => s"$c.filter(?)", new backend.Filter(_, pred))
private [parsley] final class FilterOut[A](p: Parsley[A], pred: PartialFunction[A, String])
    extends Unary[A, A](p, c => s"$c.filterOut(?)", new backend.FilterOut(_, pred))
private [parsley] final class GuardAgainst[A](p: Parsley[A], pred: PartialFunction[A, String])
    extends Unary[A, A](p, c => s"$c.guardAgainst(?)", new backend.GuardAgainst(_, pred))
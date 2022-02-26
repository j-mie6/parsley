package parsley.internal.deepembedding

import ContOps.{result, ContAdapter}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.higherKinds


private [parsley] final class Many[A](p: Parsley[A]) extends ScopedUnary[A, List[A]](p, "many", new backend.Many(_))
private [parsley] final class SkipMany[A](p: Parsley[A]) extends ScopedUnary[A, Unit](p, "skipMany", new backend.SkipMany(_))
private [parsley] final class ChainPost[A](p: Parsley[A], _op: =>Parsley[A => A])
    extends Binary[A, A => A, A](p, _op, (l, r) => s"chainPost($l, $r)", new backend.ChainPost(_, _))

// This can't be fully strict, because it depends on binary!
private [parsley] final class ChainPre[A](p: Parsley[A], op: =>Parsley[A => A])
    extends Binary[A, A => A, A](p, op, (l, r) => s"chainPre($r, $l)", new backend.ChainPre(_, _))
private [parsley] final class Chainl[A, B](init: Parsley[B], p: =>Parsley[A], op: =>Parsley[(B, A) => B])
    extends Ternary[B, A, (B, A) => B, B](init, p, op, (f, s, t) => s"chainl1($s, $t)", new backend.Chainl(_, _, _))
private [parsley] final class Chainr[A, B](p: Parsley[A], op: =>Parsley[(A, B) => B], private [Chainr] val wrap: A => B)
    extends Binary[A, (A, B) => B, B](p, op, (l, r) => s"chainr1($l, $r)", new backend.Chainr(_, _, wrap))
private [parsley] final class SepEndBy1[A, B](p: Parsley[A], sep: =>Parsley[B])
    extends Binary[A, B, List[A]](p, sep, (l, r) => s"sepEndBy1($r, $l)", new backend.SepEndBy1(_, _))
private [parsley] final class ManyUntil[A](body: Parsley[Any]) extends Unary[Any, List[A]](body, c => s"manyUntil($c)", new backend.ManyUntil(_))

private [parsley] object ManyUntil {
    object Stop
}
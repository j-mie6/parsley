package parsley.internal.deepembedding.frontend

import parsley.internal.deepembedding.ContOps, ContOps.{result, suspend, ContAdapter}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.higherKinds

import parsley.internal.deepembedding.backend, backend.StrictParsley

private [parsley] final class Many[A](p: LazyParsley[A]) extends ScopedUnary[A, List[A]](p) {
    // $COVERAGE-OFF$
    override val name: String = "many"
    override def make(p: StrictParsley[A]): StrictParsley[List[A]] = new backend.Many(p)
}
private [parsley] final class SkipMany[A](p: LazyParsley[A]) extends ScopedUnary[A, Unit](p) {
    // $COVERAGE-OFF$
    override val name: String = "skipMany"
    override def make(p: StrictParsley[A]): StrictParsley[Unit] = new backend.SkipMany(p)
}
private [parsley] final class ChainPost[A](p: LazyParsley[A], _op: =>LazyParsley[A => A]) extends Binary[A, A => A, A](p, _op) {
    // $COVERAGE-OFF$
    override def pretty(l: String, r: String): String = s"chainPost($l, $r)"
    override def make(p: StrictParsley[A], op: StrictParsley[A => A]) = new backend.ChainPost(p, op)
}

// This can't be fully strict, because it depends on binary!
private [parsley] final class ChainPre[A](p: LazyParsley[A], op: LazyParsley[A => A]) extends LazyParsley[A] {
    final override def findLetsAux[Cont[_, +_], R](seen: Set[LazyParsley[_]])(implicit ops: ContOps[Cont], state: LetFinderState): Cont[R, Unit] = {
        suspend(p.findLets[Cont, R](seen)) >> suspend(op.findLets(seen))
    }
    final override def preprocess[Cont[_, +_], R, A_ >: A](implicit ops: ContOps[Cont], lets: LetMap, recs: RecMap): Cont[R, StrictParsley[A_]] =
        for (p <- suspend(p.optimised[Cont, R, A]); op <- suspend(op.optimised[Cont, R, A => A])) yield new backend.ChainPre(p, op)
    // $COVERAGE-OFF$
    final override def prettyASTAux[Cont[_, +_]](implicit ops: ContOps[Cont]): Cont[String, String] = {
        for (p <- suspend(p.prettyASTAux); op <- suspend(op.prettyASTAux)) yield s"chainPre($p, $op)"
    }
    // $COVERAGE-ON$
}
private [parsley] final class Chainl[A, B](init: LazyParsley[B], p: =>LazyParsley[A], op: =>LazyParsley[(B, A) => B])
    extends Ternary[B, A, (B, A) => B, B](init, p, op) {
    // $COVERAGE-OFF$
    override def pretty(f: String, s: String, t: String): String = s"chainl1($s, $t)"
    // $COVERAGE-ON$
    override def make(init: StrictParsley[B], p: StrictParsley[A], op: StrictParsley[(B, A) => B]): StrictParsley[B] = new backend.Chainl(init, p, op)
}
private [parsley] final class Chainr[A, B](p: LazyParsley[A], op: =>LazyParsley[(A, B) => B], private [Chainr] val wrap: A => B)
    extends Binary[A, (A, B) => B, B](p, op)  {
    // $COVERAGE-OFF$
    override def pretty(l: String, r: String): String = s"chainl1($l, $r)"
    // $COVERAGE-ON$
    override def make(p: StrictParsley[A], op: StrictParsley[(A, B) => B]): StrictParsley[B] = new backend.Chainr(p, op, wrap)
}
private [parsley] final class SepEndBy1[A, B](p: LazyParsley[A], sep: =>LazyParsley[B]) extends Binary[A, B, List[A]](p, sep) {
    // $COVERAGE-OFF$
    override def pretty(l: String, r: String): String = s"sepEndBy1($r, $l)"
    // $COVERAGE-ON$
    override def make(p: StrictParsley[A], sep: StrictParsley[B]): StrictParsley[List[A]] = new backend.SepEndBy1(p, sep)
}
private [parsley] final class ManyUntil[A](body: LazyParsley[Any]) extends Unary[Any, List[A]](body) {
    // $COVERAGE-OFF$
    override def pretty(c: String): String = s"manyUntil($c)"
    // $COVERAGE-ON$
    override def make(p: StrictParsley[Any]): StrictParsley[List[A]] = new backend.ManyUntil(p)
}
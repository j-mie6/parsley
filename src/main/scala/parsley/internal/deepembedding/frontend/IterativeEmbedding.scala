package parsley.internal.deepembedding.frontend

import parsley.internal.deepembedding.ContOps, ContOps.{result, ContAdapter}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.higherKinds

import parsley.internal.deepembedding.backend, backend.StrictParsley

private [parsley] final class Many[A](p: LazyParsley[A]) extends ScopedUnary[A, List[A]](p) {
    override val name = "many"
    override def make(p: StrictParsley[A]) = new backend.Many(p)
}
private [parsley] final class SkipMany[A](p: LazyParsley[A]) extends ScopedUnary[A, Unit](p) {
    override val name = "skipMany"
    override def make(p: StrictParsley[A]) = new backend.SkipMany(p)
}
private [parsley] final class ChainPost[A](p: LazyParsley[A], _op: =>LazyParsley[A => A]) extends Binary[A, A => A, A](p, _op) {
    override def pretty(l: String, r: String) = s"chainPost($l, $r)"
    override def make(p: StrictParsley[A], op: StrictParsley[A => A]) = new backend.ChainPost(p, op)
}

// This can't be fully strict, because it depends on binary!
private [parsley] final class ChainPre[A](p: LazyParsley[A], op: LazyParsley[A => A]) extends LazyParsley[A] {
    final override def findLetsAux[Cont[_, +_], R](seen: Set[LazyParsley[_]])(implicit ops: ContOps[Cont], state: LetFinderState): Cont[R, Unit] = {
        p.findLets(seen) >> op.findLets(seen)
    }
    final override def preprocess[Cont[_, +_], R, A_ >: A](implicit ops: ContOps[Cont], lets: LetMap, recs: RecMap): Cont[R, StrictParsley[A_]] =
        for (p <- p.optimised; op <- op.optimised) yield new backend.ChainPre(p, op)
    // $COVERAGE-OFF$
    final override def prettyASTAux[Cont[_, +_]](implicit ops: ContOps[Cont]): Cont[String, String] = {
        for (p <- p.prettyASTAux; op <- op.prettyASTAux) yield s"chainPre($p, $op)"
    }
    // $COVERAGE-ON$
}
private [parsley] final class Chainl[A, B](init: LazyParsley[B], p: =>LazyParsley[A], op: =>LazyParsley[(B, A) => B])
    extends Ternary[B, A, (B, A) => B, B](init, p, op) {
    override def pretty(f: String, s: String, t: String) = s"chainl1($s, $t)"
    override def make(init: StrictParsley[B], p: StrictParsley[A], op: StrictParsley[(B, A) => B]) = new backend.Chainl(init, p, op)
}
private [parsley] final class Chainr[A, B](p: LazyParsley[A], op: =>LazyParsley[(A, B) => B], private [Chainr] val wrap: A => B)
    extends Binary[A, (A, B) => B, B](p, op)  {
    override def pretty(l: String, r: String) = s"chainl1($l, $r)"
    override def make(p: StrictParsley[A], op: StrictParsley[(A, B) => B]) = new backend.Chainr(p, op, wrap)
}
private [parsley] final class SepEndBy1[A, B](p: LazyParsley[A], sep: =>LazyParsley[B]) extends Binary[A, B, List[A]](p, sep) {
    override def pretty(l: String, r: String) = s"sepEndBy1($r, $l)"
    override def make(p: StrictParsley[A], sep: StrictParsley[B]) = new backend.SepEndBy1(p, sep)
}
private [parsley] final class ManyUntil[A](body: LazyParsley[Any]) extends Unary[Any, List[A]](body) {
    override def pretty(c: String) = s"manyUntil($c)"
    override def make(p: StrictParsley[Any]) = new backend.ManyUntil(p)
}
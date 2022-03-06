package parsley.internal.deepembedding.frontend

import parsley.internal.deepembedding.ContOps, ContOps.{result, ContAdapter}

import scala.language.higherKinds
import parsley.internal.deepembedding.backend, backend.StrictParsley

// Core Embedding
private [frontend] abstract class Unary[A, B](p: LazyParsley[A]) extends LazyParsley[B] {
    def pretty(p: String): String
    def make(p: StrictParsley[A]): StrictParsley[B]

    final override def findLetsAux[Cont[_, +_], R](seen: Set[LazyParsley[_]])
        (implicit ops: ContOps[Cont], state: LetFinderState): Cont[R,Unit] = p.findLets(seen)
    override def preprocess[Cont[_, +_], R, B_ >: B](implicit ops: ContOps[Cont],
                                                              lets: LetMap, recs: RecMap): Cont[R, StrictParsley[B_]] =
        for (p <- p.optimised) yield make(p)
    // $COVERAGE-OFF$
    final override def prettyASTAux[Cont[_, +_]](implicit ops: ContOps[Cont]): Cont[String, String] = for (c <- p.prettyASTAux) yield pretty(c)
    // $COVERAGE-ON$
}

private [frontend] abstract class ScopedUnary[A, B](_p: LazyParsley[A]) extends Unary[A, B](_p) {
    def name: String
    final def pretty(c: String) = s"$name($c)"
}

private [frontend] abstract class Binary[A, B, C](left: LazyParsley[A], _right: =>LazyParsley[B]) extends LazyParsley[C] {
    private lazy val right = _right

    def pretty(p: String, q: String): String
    def make(p: StrictParsley[A], q: StrictParsley[B]): StrictParsley[C]

    final override def findLetsAux[Cont[_, +_], R](seen: Set[LazyParsley[_]])
        (implicit ops: ContOps[Cont], state: LetFinderState): Cont[R,Unit] = {
        left.findLets(seen) >> right.findLets(seen)
    }
    final override def preprocess[Cont[_, +_], R, C_ >: C](implicit ops: ContOps[Cont],
                                                                    lets: LetMap, recs: RecMap): Cont[R, StrictParsley[C_]] =
        for (left <- left.optimised; right <- right.optimised) yield make(left, right)
    // $COVERAGE-OFF$
    final override def prettyASTAux[Cont[_, +_]](implicit ops: ContOps[Cont]): Cont[String, String] = {
        for (l <- left.prettyASTAux; r <- right.prettyASTAux) yield pretty(l, r)
    }
    // $COVERAGE-ON$
}

private [frontend] abstract class Ternary[A, B, C, D](first: LazyParsley[A], _second: =>LazyParsley[B], _third: =>LazyParsley[C]) extends LazyParsley[D] {
    private lazy val second: LazyParsley[B] = _second
    private lazy val third: LazyParsley[C] = _third

    def pretty(p: String, q: String, r: String): String
    def make(p: StrictParsley[A], q: StrictParsley[B], r: StrictParsley[C]): StrictParsley[D]

    final override def findLetsAux[Cont[_, +_], R](seen: Set[LazyParsley[_]])
        (implicit ops: ContOps[Cont], state: LetFinderState): Cont[R, Unit] = {
        first.findLets[Cont, R](seen) >> second.findLets(seen) >> third.findLets(seen)
    }
    final override def preprocess[Cont[_, +_], R, D_ >: D](implicit ops: ContOps[Cont],
                                                                    lets: LetMap, recs: RecMap): Cont[R, StrictParsley[D_]] =
        for (first <- first.optimised; second <- second.optimised; third <- third.optimised) yield make(first, second, third)
    // $COVERAGE-OFF$
    final override def prettyASTAux[Cont[_, +_]](implicit ops: ContOps[Cont]): Cont[String, String] =
        for (f <- first.prettyASTAux; s <- second.prettyASTAux; t <- third.prettyASTAux) yield pretty(f, s, t)
    // $COVERAGE-ON$
}
package parsley.internal.deepembedding

import ContOps.{result, ContAdapter}

import scala.language.higherKinds
import backend.StrictParsley

// Core Embedding
private [parsley] abstract class Singleton[A](pretty: String, strict: StrictParsley[A]) extends Parsley[A] {
    final override def findLetsAux[Cont[_, +_], R](seen: Set[Parsley[_]])
        (implicit ops: ContOps[Cont], state: LetFinderState): Cont[R, Unit] = result(())
    final override def preprocess[Cont[_, +_], R, A_ >: A](implicit ops: ContOps[Cont],
                                                                    lets: LetMap, recs: RecMap): Cont[R, StrictParsley[A_]] = result(strict)
    // $COVERAGE-OFF$
    final override def prettyASTAux[Cont[_, +_]](implicit ops: ContOps[Cont]): Cont[String, String] = result(pretty)
    // $COVERAGE-ON$
}

private [deepembedding] abstract class Unary[A, B](p: Parsley[A], pretty: String => String, make: StrictParsley[A] => StrictParsley[B])
    extends Parsley[B] {
    final override def findLetsAux[Cont[_, +_], R](seen: Set[Parsley[_]])
        (implicit ops: ContOps[Cont], state: LetFinderState): Cont[R,Unit] = p.findLets(seen)
    override def preprocess[Cont[_, +_], R, B_ >: B](implicit ops: ContOps[Cont],
                                                              lets: LetMap, recs: RecMap): Cont[R, StrictParsley[B_]] =
        for (p <- p.optimised) yield make(p)
    // $COVERAGE-OFF$
    final override def prettyASTAux[Cont[_, +_]](implicit ops: ContOps[Cont]): Cont[String, String] = for (c <- p.prettyASTAux) yield pretty(c)
    // $COVERAGE-ON$
}

private [deepembedding] abstract class ScopedUnary[A, B](_p: Parsley[A], name: String, make: StrictParsley[A] => StrictParsley[B])
    extends Unary[A, B](_p, c => s"$name($c)", make)

private [deepembedding] abstract class Binary[A, B, C](left: Parsley[A], _right: =>Parsley[B],
                                                       pretty: (String, String) => String, make: (StrictParsley[A], StrictParsley[B]) => StrictParsley[C])
    extends Parsley[C] {
    private lazy val right = _right
    final override def findLetsAux[Cont[_, +_], R](seen: Set[Parsley[_]])
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

private [deepembedding] abstract class Ternary[A, B, C, D](first: Parsley[A], _second: =>Parsley[B], _third: =>Parsley[C],
                                                           pretty: (String, String, String) => String,
                                                           make: (StrictParsley[A], StrictParsley[B], StrictParsley[C]) => StrictParsley[D])
    extends Parsley[D] {
    private lazy val second: Parsley[B] = _second
    private lazy val third: Parsley[C] = _third
    final override def findLetsAux[Cont[_, +_], R](seen: Set[Parsley[_]])
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
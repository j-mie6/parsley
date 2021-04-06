package parsley.internal.deepembedding

import ContOps.{result, ContAdapter}
import parsley.internal.machine.instructions

import scala.language.higherKinds

// Core Embedding
private [parsley] abstract class Singleton[A](pretty: String, instr: =>instructions.Instr) extends Parsley[A] {
    final override def findLetsAux[Cont[_, +_], R]
        (implicit ops: ContOps[Cont, R], seen: Set[Parsley[_]], state: LetFinderState): Cont[R, Unit] = result(())
    final override def preprocess[Cont[_, +_], R, A_ >: A](implicit ops: ContOps[Cont, R], seen: Set[Parsley[_]], sub: SubMap, recs: RecMap): Cont[R, Parsley[A_]] = result(this)
    final override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont, R], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        result(instrs += instr)
    }
    // $COVERAGE-OFF$
    final override def prettyASTAux[Cont[_, +_], R](implicit ops: ContOps[Cont, R]): Cont[R, String] = result(pretty)
    // $COVERAGE-ON$
}

private [deepembedding] abstract class Unary[A, B](__p: =>Parsley[A])(pretty: String => String, empty: =>Unary[A, B]) extends Parsley[B] {
    private lazy val _p = __p
    private [deepembedding] var p: Parsley[A] = _
    protected val childRepeats: Int = 1
    protected val numInstrs: Int
    final override def findLetsAux[Cont[_, +_], R]
        (implicit ops: ContOps[Cont, R], seen: Set[Parsley[_]], state: LetFinderState): Cont[R,Unit] = _p.findLets()
    override def preprocess[Cont[_, +_], R, B_ >: B](implicit ops: ContOps[Cont, R], seen: Set[Parsley[_]], sub: SubMap, recs: RecMap): Cont[R, Parsley[B_]] =
        for (p <- _p.optimised) yield empty.ready(p)
    private [deepembedding] def ready(p: Parsley[A]): this.type = {
        processed = true
        this.p = p
        size = p.size + numInstrs
        this
    }
    // $COVERAGE-OFF$
    final override def prettyASTAux[Cont[_, +_], R](implicit ops: ContOps[Cont, R]): Cont[R, String] = for (c <- p.prettyASTAux) yield pretty(c)
    // $COVERAGE-ON$
}

private [deepembedding] abstract class Binary[A, B, C](__left: =>Parsley[A], __right: =>Parsley[B])
                                                      (pretty: (String, String) => String, empty: =>Binary[A, B, C]) extends Parsley[C] {
    private lazy val _left = __left
    private lazy val _right = __right
    private [deepembedding] var left: Parsley[A] = _
    private [deepembedding] var right: Parsley[B] = _
    protected val numInstrs: Int
    protected val leftRepeats: Int = 1
    protected val rightRepeats: Int = 1
    final override def findLetsAux[Cont[_, +_], R]
        (implicit ops: ContOps[Cont, R], seen: Set[Parsley[_]], state: LetFinderState): Cont[R,Unit] = {
        _left.findLets() >> _right.findLets()
    }
    final override def preprocess[Cont[_, +_], R, C_ >: C](implicit ops: ContOps[Cont, R], seen: Set[Parsley[_]], sub: SubMap, recs: RecMap): Cont[R, Parsley[C_]] =
        for (left <- _left.optimised; right <- _right.optimised) yield {
            empty.ready(left, right)
        }
    private [deepembedding] def ready(left: Parsley[A], right: Parsley[B]): this.type = {
        processed = true
        this.left = left
        this.right = right
        size = leftRepeats * left.size + rightRepeats * right.size + numInstrs
        this
    }
    // $COVERAGE-OFF$
    final override def prettyASTAux[Cont[_, +_], R](implicit ops: ContOps[Cont, R]): Cont[R, String] = {
        for (l <- left.prettyASTAux; r <- right.prettyASTAux) yield pretty(l, r)
    }
    // $COVERAGE-ON$
}

private [deepembedding] abstract class Ternary[A, B, C, D](__first: =>Parsley[A], __second: =>Parsley[B], __third: =>Parsley[C])
                                                          (pretty: (String, String, String) => String, empty: =>Ternary[A, B, C, D]) extends Parsley[D] {
    private lazy val _first: Parsley[A] = __first
    private lazy val _second: Parsley[B] = __second
    private lazy val _third: Parsley[C] = __third
    private [deepembedding] var first: Parsley[A] = _
    private [deepembedding] var second: Parsley[B] = _
    private [deepembedding] var third: Parsley[C] = _
    protected val numInstrs: Int
    final override def findLetsAux[Cont[_, +_], R]
        (implicit ops: ContOps[Cont, R], seen: Set[Parsley[_]], state: LetFinderState): Cont[R, Unit] = {
        _first.findLets() >> _second.findLets() >> _third.findLets()
    }
    final override def preprocess[Cont[_, +_], R, D_ >: D](implicit ops: ContOps[Cont, R], seen: Set[Parsley[_]], sub: SubMap, recs: RecMap): Cont[R, Parsley[D_]] =
        for (first <- _first.optimised; second <- _second.optimised; third <- _third.optimised) yield {
            empty.ready(first, second, third)
        }
    private [deepembedding] def ready(first: Parsley[A], second: Parsley[B], third: Parsley[C]): this.type = {
        processed = true
        this.first = first
        this.second = second
        this.third = third
        size = first.size + second.size + third.size + numInstrs
        this
    }
    // $COVERAGE-OFF$
    final override def prettyASTAux[Cont[_, +_], R](implicit ops: ContOps[Cont, R]): Cont[R, String] =
        for (f <- first.prettyASTAux; s <- second.prettyASTAux; t <- third.prettyASTAux) yield pretty(f, s, t)
    // $COVERAGE-ON$
}
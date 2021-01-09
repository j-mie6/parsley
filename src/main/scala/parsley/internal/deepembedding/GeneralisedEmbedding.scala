package parsley.internal.deepembedding

import ContOps.{result, ContAdapter}
import parsley.internal.{UnsafeOption, instructions}

import scala.language.higherKinds

// Core Embedding
private [parsley] abstract class Singleton[A](pretty: String, instr: =>instructions.Instr) extends Parsley[A] {
    final override def preprocess[Cont[_, +_]: ContOps, A_ >: A](implicit seen: Set[Parsley[_]], sub: SubMap,
                                                                 label: UnsafeOption[String]): Cont[Unit, Parsley[A_]] = result(this)
    final override def findLetsAux[Cont[_, +_]: ContOps](implicit seen: Set[Parsley[_]], state: LetFinderState): Cont[Unit, Unit] = result(())
    final override def codeGen[Cont[_, +_]: ContOps](implicit instrs: InstrBuffer, state: CodeGenState): Cont[Unit, Unit] = {
        result(instrs += instr)
    }
    // $COVERAGE-OFF$
    final override def prettyASTAux[Cont[_, +_]: ContOps]: Cont[String, String] = result(pretty)
    // $COVERAGE-ON$
}

private [deepembedding] abstract class SingletonExpect[A](pretty: String, builder: UnsafeOption[String] => SingletonExpect[A], instr: instructions.Instr)
    extends Parsley[A] {
    final override def preprocess[Cont[_, +_]: ContOps, A_ >: A](implicit seen: Set[Parsley[_]], sub: SubMap,
                                                           label: UnsafeOption[String]): Cont[Unit, Parsley[A]] = {
        if (label == null) result(this)
        else result(builder(label))
    }
    final override def findLetsAux[Cont[_, +_]: ContOps](implicit seen: Set[Parsley[_]], state: LetFinderState): Cont[Unit, Unit] = result(())
    final override def codeGen[Cont[_, +_]: ContOps](implicit instrs: InstrBuffer, state: CodeGenState): Cont[Unit, Unit] = {
        result(instrs += instr)
    }
    // $COVERAGE-OFF$
    final override def prettyASTAux[Cont[_, +_]: ContOps]: Cont[String, String] = result(pretty)
    // $COVERAGE-ON$
}

private [deepembedding] abstract class Unary[A, B](_p: =>Parsley[A])(pretty: String => String, empty: String => Unary[A, B]) extends Parsley[B] {
    private [deepembedding] var p: Parsley[A] = _
    protected val childRepeats: Int = 1
    protected val numInstrs: Int
    override def findLetsAux[Cont[_, +_]: ContOps](implicit seen: Set[Parsley[_]], state: LetFinderState): Cont[Unit,Unit] = {
        processed = false
        p = _p
        p.findLets
    }
    override def preprocess[Cont[_, +_]: ContOps, B_ >: B](implicit seen: Set[Parsley[_]], sub: SubMap,
                                                   label: UnsafeOption[String]): Cont[Unit, Parsley[B_]] =
        if (label == null && processed) result(this) else for (p <- this.p.optimised) yield {
            val self = if (label == null) this else empty(label)
            self.ready(p)
        }
    private [deepembedding] def ready(p: Parsley[A]): this.type = {
        processed = true
        this.p = p
        size = p.size + numInstrs
        this
    }
    // $COVERAGE-OFF$
    override def prettyASTAux[Cont[_, +_]: ContOps]: Cont[String,String] = for (c <- p.prettyASTAux) yield pretty(c)
    // $COVERAGE-ON$
}

private [deepembedding] abstract class Binary[A, B, C](_left: =>Parsley[A], _right: =>Parsley[B])(pretty: (String, String) => String, empty: =>Binary[A, B, C])
    extends Parsley[C] {
    private [deepembedding] var left: Parsley[A] = _
    private [deepembedding] var right: Parsley[B] = _
    protected val numInstrs: Int
    protected val leftRepeats: Int = 1
    protected val rightRepeats: Int = 1
    override def findLetsAux[Cont[_, +_]: ContOps](implicit seen: Set[Parsley[_]], state: LetFinderState): Cont[Unit,Unit] = {
        processed = false
        left = _left
        right = _right
        left.findLets >> right.findLets
    }
    override def preprocess[Cont[_, +_]: ContOps, C_ >: C](implicit seen: Set[Parsley[_]], sub: SubMap,
                                                   label: UnsafeOption[String]): Cont[Unit, Parsley[C_]] =
        if (label == null && processed) result(this) else for (left <- this.left.optimised; right <- this.right.optimised) yield {
            val self = if (label == null) this else empty
            self.ready(left, right)
        }
    private [deepembedding] def ready(left: Parsley[A], right: Parsley[B]): this.type = {
        processed = true
        this.left = left
        this.right = right
        size = leftRepeats * left.size + rightRepeats * right.size + numInstrs
        this
    }
    // $COVERAGE-OFF$
    override def prettyASTAux[Cont[_, +_]: ContOps]: Cont[String,String] = {
        for (l <- left.prettyASTAux; r <- right.prettyASTAux) yield pretty(l, r)
    }
    // $COVERAGE-ON$
}

private [deepembedding] abstract class Ternary[A, B, C, D](_first: =>Parsley[A], _second: =>Parsley[B], _third: =>Parsley[C])
                                                          (pretty: (String, String, String) => String, empty: =>Ternary[A, B, C, D]) extends Parsley[D] {
    private [deepembedding] var first: Parsley[A] = _
    private [deepembedding] var second: Parsley[B] = _
    private [deepembedding] var third: Parsley[C] = _
    protected val numInstrs: Int
    override def preprocess[Cont[_, +_]: ContOps, D_ >: D](implicit seen: Set[Parsley[_]], sub: SubMap,
                                                           label: UnsafeOption[String]): Cont[Unit, Parsley[D_]] =
        if (label == null && processed) result(this) else
            for (first <- this.first.optimised; second <- this.second.optimised; third <- this.third.optimised) yield {
                val self = if (label == null) this else empty
                self.ready(first, second, third)
            }
    private [deepembedding] def ready(first: Parsley[A], second: Parsley[B], third: Parsley[C]): this.type = {
        processed = true
        this.first = first
        this.second = second
        this.third = third
        size = first.size + second.size + third.size + numInstrs
        this
    }
    override def findLetsAux[Cont[_, +_]: ContOps](implicit seen: Set[Parsley[_]], state: LetFinderState): Cont[Unit, Unit] = {
        processed = false
        first = _first
        second = _second
        third = _third
        first.findLets >> second.findLets >> third.findLets
    }
    // $COVERAGE-OFF$
    override def prettyASTAux[Cont[_, +_]: ContOps]: Cont[String, String] =
        for (f <- first.prettyASTAux; s <- second.prettyASTAux; t <- third.prettyASTAux) yield pretty(f, s, t)
    // $COVERAGE-ON$
}
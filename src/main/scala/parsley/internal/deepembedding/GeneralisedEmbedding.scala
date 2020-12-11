package parsley.internal.deepembedding

import ContOps.{result, ContAdapter}
import parsley.internal.{UnsafeOption, instructions}

import scala.language.higherKinds

// Core Embedding
private [parsley] abstract class Singleton[A](pretty: String, instr: instructions.Instr) extends Parsley[A] {
    final override def preprocess[Cont[_, +_], A_ >: A](implicit seen: Set[Parsley[_]], sub: SubMap,
                                                           label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[A_]] = result(this)
    final override def findLetsAux[Cont[_, +_]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] = result(())
    final override def codeGen[Cont[_, +_]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] = {
        result(instrs += instr)
    }
    final override def prettyASTAux[Cont[_, +_]](implicit ops: ContOps[Cont]): Cont[String, String] = result(pretty)
}

private [deepembedding] abstract class SingletonExpect[A](pretty: String, builder: UnsafeOption[String] => SingletonExpect[A], instr: instructions.Instr)
    extends Parsley[A] {
    final override def preprocess[Cont[_, +_], A_ >: A](implicit seen: Set[Parsley[_]], sub: SubMap,
                                                           label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[A]] = {
        if (label == null) result(this)
        else result(builder(label))
    }
    final override def findLetsAux[Cont[_, +_]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] = result(())
    final override def codeGen[Cont[_, +_]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] = {
        result(instrs += instr)
    }
    final override def prettyASTAux[Cont[_, +_]](implicit ops: ContOps[Cont]): Cont[String, String] = result(pretty)
}

private [deepembedding] abstract class Unary[A, B](_p: =>Parsley[A])(pretty: String => String, empty: String => Unary[A, B]) extends Parsley[B] {
    protected var p: Parsley[A] = _
    protected val childRepeats: Int = 1
    protected val numInstrs: Int
    override def findLetsAux[Cont[_, +_]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit,Unit] = {
        processed = false
        p = _p
        p.findLets
    }
    override def preprocess[Cont[_, +_], B_ >: B](implicit seen: Set[Parsley[_]], sub: SubMap,
                                                   label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[B_]] =
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
    override def prettyASTAux[Cont[_, +_]](implicit ops: ContOps[Cont]): Cont[String,String] = for (c <- p.prettyASTAux) yield pretty(c)
}

private [deepembedding] abstract class Binary[A, B, C](_left: =>Parsley[A], _right: =>Parsley[B])(pretty: (String, String) => String, empty: =>Binary[A, B, C])
    extends Parsley[C] {
    protected var left: Parsley[A] = _
    protected var right: Parsley[B] = _
    protected val numInstrs: Int
    protected val leftRepeats: Int = 1
    protected val rightRepeats: Int = 1
    override def findLetsAux[Cont[_, +_]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit,Unit] = {
        processed = false
        left = _left
        right = _right
        left.findLets >> right.findLets
    }
    override def preprocess[Cont[_, +_], C_ >: C](implicit seen: Set[Parsley[_]], sub: SubMap,
                                                   label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[C_]] =
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
    override def prettyASTAux[Cont[_, +_]](implicit ops: ContOps[Cont]): Cont[String,String] = {
        for (l <- left.prettyASTAux; r <- right.prettyASTAux) yield pretty(l, r)
    }
}

private [deepembedding] abstract class Ternary[A, B, C, D](_first: =>Parsley[A], _second: =>Parsley[B], _third: =>Parsley[C])
                                                          (pretty: (String, String, String) => String, empty: =>Ternary[A, B, C, D]) extends Parsley[D] {
    protected var first: Parsley[A] = _
    protected var second: Parsley[B] = _
    protected var third: Parsley[C] = _
    protected val numInstrs: Int
    override def preprocess[Cont[_, +_], D_ >: D](implicit seen: Set[Parsley[_]], sub: SubMap,
                                                           label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[D_]] =
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
    override def findLetsAux[Cont[_, +_]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] = {
        processed = false
        first = _first
        second = _second
        third = _third
        first.findLets >> second.findLets >> third.findLets
    }
    override def prettyASTAux[Cont[_, +_]](implicit ops: ContOps[Cont]): Cont[String, String] =
        for (f <- first.prettyASTAux; s <- second.prettyASTAux; t <- third.prettyASTAux) yield pretty(f, s, t)
}
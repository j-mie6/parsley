package parsley.internal.deepembedding

import ContOps.{result, ContAdapter}
import parsley.internal.machine.instructions

import scala.language.higherKinds

import backend.StrictParsley

// Core Embedding
private [parsley] abstract class Singleton[A](pretty: String, instr: =>instructions.Instr) extends Parsley[A] {
    final override def findLetsAux[Cont[_, +_], R]
        (implicit ops: ContOps[Cont, R], seen: Set[Parsley[_]], state: LetFinderState): Cont[R, Unit] = result(())
    final override def preprocess[Cont[_, +_], R, A_ >: A](implicit ops: ContOps[Cont, R], seen: Set[Parsley[_]],
                                                                    lets: LetMap, recs: RecMap): Cont[R, Parsley[A_]] = result(this)
    final override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont, R], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        result(instrs += instr)
    }
    // $COVERAGE-OFF$
    final override def prettyASTAux[Cont[_, +_], R](implicit ops: ContOps[Cont, R]): Cont[R, String] = result(pretty)
    // $COVERAGE-ON$
}

private [deepembedding] abstract class Unary[A, B](private [deepembedding] var p: Parsley[A], pretty: String => String, make: StrictParsley[A] => Unary[A, B])
    extends Parsley[B] {
    protected val childRepeats: Int = 1
    protected val numInstrs: Int
    final override def findLetsAux[Cont[_, +_], R]
        (implicit ops: ContOps[Cont, R], seen: Set[Parsley[_]], state: LetFinderState): Cont[R,Unit] = p.findLets
    override def preprocess[Cont[_, +_], R, B_ >: B](implicit ops: ContOps[Cont, R], seen: Set[Parsley[_]],
                                                              lets: LetMap, recs: RecMap): Cont[R, Parsley[B_]] =
        for (p <- p.optimised) yield make(p).ready()
    private [deepembedding] def ready(): this.type = {
        processed = true
        size = p.size + numInstrs
        this
    }
    // $COVERAGE-OFF$
    final override def prettyASTAux[Cont[_, +_], R](implicit ops: ContOps[Cont, R]): Cont[R, String] = for (c <- p.prettyASTAux) yield pretty(c)
    // $COVERAGE-ON$
}

private [deepembedding] abstract class ScopedUnary[A, B](_p: Parsley[A], name: String, make: StrictParsley[A] => ScopedUnary[A, B],
                                                         setup: Int => instructions.Instr, instr: instructions.Instr)
    extends Unary[A, B](_p, c => s"$name($c)", make) {
    final override val numInstrs = 2
    final override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont, R], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        val handler = state.freshLabel()
        instrs += setup(handler)
        p.codeGen |> {
            instrs += new instructions.Label(handler)
            instrs += instr
        }
    }
}

private [deepembedding] abstract class ScopedUnaryWithState[A, B](_p: Parsley[A], name: String, doesNotProduceHints: Boolean,
                                                                  make: StrictParsley[A] => ScopedUnary[A, B], instr: instructions.Instr)
    extends ScopedUnary[A, B](_p, name, make, new instructions.PushHandlerAndState(_, doesNotProduceHints, doesNotProduceHints), instr)

private [deepembedding] abstract class Binary[A, B, C](private [deepembedding] var left: Parsley[A], __right: =>Parsley[B],
                                                       pretty: (String, String) => String, make: StrictParsley[A] => Binary[A, B, C]) extends Parsley[C] {
    private lazy val _right = __right
    private [deepembedding] var right: StrictParsley[B] = _
    protected val numInstrs: Int
    protected val leftRepeats: Int = 1
    protected val rightRepeats: Int = 1
    final override def findLetsAux[Cont[_, +_], R]
        (implicit ops: ContOps[Cont, R], seen: Set[Parsley[_]], state: LetFinderState): Cont[R,Unit] = {
        left.findLets >> _right.findLets
    }
    final override def preprocess[Cont[_, +_], R, C_ >: C](implicit ops: ContOps[Cont, R], seen: Set[Parsley[_]],
                                                                    lets: LetMap, recs: RecMap): Cont[R, Parsley[C_]] =
        for (left <- left.optimised; right <- _right.optimised) yield {
            make(left).ready(right)
        }
    private [deepembedding] def ready(right: StrictParsley[B]): this.type = {
        processed = true
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

private [deepembedding] abstract class Ternary[A, B, C, D](private [deepembedding] var first: Parsley[A], __second: =>Parsley[B], __third: =>Parsley[C],
                                                           pretty: (String, String, String) => String, make: StrictParsley[A] => Ternary[A, B, C, D])
    extends Parsley[D] {
    private lazy val _second: Parsley[B] = __second
    private lazy val _third: Parsley[C] = __third
    private [deepembedding] var second: StrictParsley[B] = _
    private [deepembedding] var third: StrictParsley[C] = _
    protected val numInstrs: Int
    final override def findLetsAux[Cont[_, +_], R]
        (implicit ops: ContOps[Cont, R], seen: Set[Parsley[_]], state: LetFinderState): Cont[R, Unit] = {
        first.findLets >> _second.findLets >> _third.findLets
    }
    final override def preprocess[Cont[_, +_], R, D_ >: D](implicit ops: ContOps[Cont, R], seen: Set[Parsley[_]],
                                                                    lets: LetMap, recs: RecMap): Cont[R, Parsley[D_]] =
        for (first <- first.optimised; second <- _second.optimised; third <- _third.optimised) yield {
            make(first).ready(second, third)
        }
    private [deepembedding] def ready(second: StrictParsley[B], third: StrictParsley[C]): this.type = {
        processed = true
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
package parsley.internal.deepembedding.frontend

import scala.annotation.tailrec
import scala.util.Random

import org.scalatest.flatspec.AnyFlatSpec
import parsley.Parsley
import parsley.combinator.ifP
import parsley.debugger.combinator.attachDebugger
import parsley.expr.chain
import parsley.internal.deepembedding.frontend.debugger.Debugged
import parsley.internal.deepembedding.singletons

class TotalAttachmentSpec extends AnyFlatSpec {
    private final class RandomParserCreator(maxDepth: Int) {
        private val rng: Random = new Random

        private val unaryModifiers: Array[Parsley[Unit] => Parsley[Unit]] = Array(
            Parsley.attempt,
            Parsley.lookAhead,
            _.map(identity[Unit]),
            _.flatMap(Parsley.pure),
            chain.left(_, Parsley.fresh { (_: Unit, _: Unit) => () }, ()),
            chain.right(_, Parsley.fresh { (_: Unit, _: Unit) => () }, ())
        )

        private val binaryModifiers: Array[(Parsley[Unit], Parsley[Unit]) => Parsley[Unit]] = Array(
            (px, py) => px <|> py,
            (px, py) => ifP(Parsley.pure(true), px, py),
        )

        private val baseParser: Parsley[Unit] = Parsley.unit

        def generate(depth: Int): Parsley[Unit] =
            if (depth >= maxDepth) baseParser
            else {
                rng.nextDouble() match {
                    case d if d <= 0.5 =>
                        val mod = unaryModifiers(rng.nextInt(unaryModifiers.length))
                        mod(generate(depth + 1))
                    case _             =>
                        val mod = binaryModifiers(rng.nextInt(binaryModifiers.length))
                        mod(generate(depth + 1), generate(depth + 1))
                }
            }
    }

    sealed trait ConstUnit[+A]
    object CUnit extends ConstUnit[Nothing]

    private final class AttachmentInspector extends GenericLazyParsleyIVisitor[Boolean, ConstUnit] {
        def failure(msg: String = "Parent parser was not debugged."): Nothing = fail(msg)

        override def visitSingleton[A](self: singletons.Singleton[A], context: Boolean): ConstUnit[A] =
            if (context) CUnit else failure()

        override def visitUnary[A, B](self: Unary[A, B], context: Boolean)(p: LazyParsley[A]): ConstUnit[B] =
            if (context) {
                val _ = visitUnknown(p, context = false)
                CUnit
            } else failure()

        override def visitBinary[A, B, C](self: Binary[A, B, C], context: Boolean)(l: LazyParsley[A], r: => LazyParsley[B]): ConstUnit[C] =
            if (context) {
                val _ = visitUnknown(l, context = false)
                val _ = visitUnknown(r, context = false)
                CUnit
            } else failure()

        override def visitTernary[A, B, C, D](self: Ternary[A, B, C, D], context: Boolean)(f: LazyParsley[A],
                                                                                           s: => LazyParsley[B],
                                                                                           t: => LazyParsley[C]): ConstUnit[D] =
            if (context) {
                val _ = visitUnknown(f, context = false)
                val _ = visitUnknown(s, context = false)
                val _ = visitUnknown(t, context = false)
                CUnit
            } else failure()

        override def visit[A](self: <|>[A], context: Boolean)(p: LazyParsley[A], q: LazyParsley[A]): ConstUnit[A] =
            if (context) {
                val _ = visitUnknown(p, context = false)
                val _ = visitUnknown(q, context = false)
                CUnit
            } else failure()

        override def visit[A](self: ChainPre[A], context: Boolean)(p: LazyParsley[A], op: LazyParsley[A => A]): ConstUnit[A] =
            if (context) {
                val _ = visitUnknown(p, context = false)
                val _ = visitUnknown(op, context = false)
                CUnit
            } else failure()

        @tailrec override def visitUnknown[A](self: LazyParsley[A], context: Boolean): ConstUnit[A] =
            self match {
                case d: Debugged[A] if !context => visitUnknown(d.par.get, context = true)
                case _: Debugged[A]             => failure("Not allowed to stack debuggers.") // Can't have a debugged on top of another!
                case s: singletons.Singleton[A] => visitSingleton(s, context)
                case g: GenericLazyParsley[A]   => visitGeneric(g, context)
                case alt: <|>[A]                => alt.visit(this, context)
                case cpre: ChainPre[A]          => cpre.visit(this, context)
                case _                          => if (context) CUnit else failure()
            }
    }

    behavior of "the debug node attachment visitor"

    it should "attach debuggers to all nodes of a parser" in {
        val verifier = new AttachmentInspector

        for (i <- 0 until 12) {
            val parserGenerator = new RandomParserCreator(i)
            for (_ <- 0 until 100) {
                val (_, dbg) = attachDebugger(parserGenerator.generate(0))
                dbg.internal match {
                    case seq: *>[_] => verifier.visitUnknown(seq.right, context = false)
                    case _          => fail("Debugger not attached.")
                }
            }
        }
    }
}

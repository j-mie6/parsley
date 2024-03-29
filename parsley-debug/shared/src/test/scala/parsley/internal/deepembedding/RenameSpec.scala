/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding

import org.scalatest.Assertions.fail
import parsley.ParsleyTest
import parsley.debugger.DebuggerUsageSpec
import parsley.debugger.internal.{DebugContext, Rename}
import parsley.debugger.util.Collector
import parsley.internal.deepembedding.backend.StrictParsley
import parsley.internal.deepembedding.frontend.{LazyParsley, LazyParsleyIVisitor, LetFinderState, LetMap}
import parsley.internal.deepembedding.frontend.debugger.Debugged
import parsley.token.Lexer
import parsley.token.descriptions.LexicalDesc

class RenameSpec extends ParsleyTest {
    behavior of "the Renamer object"

    it should "not rename a parser it does not know of" in {
        val exampleParser = new DummyParser
        Rename(None, exampleParser) shouldNot be("exampleParser")
    }

    it should "rename a parser it is aware of" in {
        val exampleParser: LazyParsley[_] = new DummyParser
        Rename.addNames(Map((exampleParser, "exampleParser")))

        Rename(None, exampleParser) shouldBe "exampleParser"
    }

    it should "translate a symbolic parser's name" in {
        val symbolic = new <**>
        Rename(None, symbolic) shouldBe "<**>"
    }

    it should "pass through Debugged parsers and get the inner parser's name" in {
        val symbolic = new <**>
        val debugged = new Debugged[Any](symbolic, Some(symbolic), None)(new DebugContext())

        Rename(None, debugged) shouldBe "<**>"
    }

    it should "override a known name if a parser is named." in {
        val exampleParser: LazyParsley[_] = new DummyParser
        Rename.addNames(Map((exampleParser, "exampleParser")))

        Rename(Some("knownName"), exampleParser) shouldBe "knownName"
    }

    behavior of "the Collector implementations"

    it should "collect names of parsers from objects (on supported platforms)" in {
        if (Collector.isSupported) {
            Collector.names(DebuggerUsageSpec.Arithmetic)
            Rename(None, DebuggerUsageSpec.Arithmetic.prog.internal) shouldBe "prog"

            info("it should also allow overriding the name")
            Collector.assignName(DebuggerUsageSpec.Arithmetic.prog, "foo")
            Rename(None, DebuggerUsageSpec.Arithmetic.prog.internal) shouldBe "foo"
        } else {
            alert("the current platform does not support Collector")
        }
    }

    it should "collect names of parsers from lexers (on supported platforms)" in {
        val lexer = new Lexer(LexicalDesc.plain.copy())
        if (Collector.isSupported) {
            Collector.lexer(lexer)
            Rename(None, lexer.lexeme.names.identifier.internal) shouldBe "identifier"
        } else {
            alert("the current platform does not support Collector")
        }
    }
}

object RenameSpec {
    def crash(): Nothing = fail("Should not have been run.")
}

// These are dummy parsers used for the above tests.
// We don't actually care that they don't implement anything.
private class DummyParser extends LazyParsley[Any] {
    override protected def findLetsAux[M[_, +_] : ContOps, R]
    (seen: Set[LazyParsley[_]])(implicit state: LetFinderState): M[R, Unit] =
        RenameSpec.crash()

    override protected def preprocess[M[_, +_] : ContOps, R, A_ >: Any]
    (implicit lets: LetMap): M[R, StrictParsley[A_]] =
        RenameSpec.crash()

    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[Any] =
        visitor.visitUnknown(this, context)

    override private [parsley] def prettyName = "dummyParser"
}

private class <**> extends LazyParsley[Any] {
    override protected def findLetsAux[M[_, +_] : ContOps, R]
    (seen: Set[LazyParsley[_]])(implicit state: LetFinderState): M[R, Unit] =
        RenameSpec.crash()

    override protected def preprocess[M[_, +_] : ContOps, R, A_ >: Any]
    (implicit lets: LetMap): M[R, StrictParsley[A_]] =
        RenameSpec.crash()

    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[Any] =
        visitor.visitUnknown(this, context)

    override private [parsley] def prettyName = "<**>"
}

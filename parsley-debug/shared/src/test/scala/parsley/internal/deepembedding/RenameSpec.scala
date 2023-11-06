/*
 * Copyright (c) 2020, Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors
 *    may be used to endorse or promote products derived from this software without
 *    specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package parsley.internal.deepembedding

import org.scalatest.Assertions.fail
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import parsley.debugger.internal.{DebugContext, Rename}
import parsley.internal.deepembedding.backend.StrictParsley
import parsley.internal.deepembedding.frontend.{LazyParsley, LazyParsleyIVisitor, LetFinderState, LetMap, RecMap}
import parsley.internal.deepembedding.frontend.debugger.Debugged

class RenameSpec extends AnyFlatSpec with Matchers {
    behavior of "the Renamer object"

    it should "not rename a parser it does not know of" in {
        val exampleParser = new DummyParser
        Rename(None, exampleParser) shouldNot be("exampleParser")
    }

    it should "rename a parser it is aware of" in {
        val exampleParser: LazyParsley[_] = new DummyParser
        Rename.addNames(Map(exampleParser -> "exampleParser"))

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
        Rename.addNames(Map(exampleParser -> "exampleParser"))

        Rename(Some("knownName"), exampleParser) shouldBe "knownName"
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
    (implicit lets: LetMap, recs: RecMap): M[R, StrictParsley[A_]] =
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
    (implicit lets: LetMap, recs: RecMap): M[R, StrictParsley[A_]] =
        RenameSpec.crash()

    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[Any] =
        visitor.visitUnknown(this, context)

    override private [parsley] def prettyName = "<**>"
}

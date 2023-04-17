/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import parsley.debugger.internal.{DebugContext, Rename}
import parsley.internal.deepembedding.backend.StrictParsley
import parsley.internal.deepembedding.frontend.{LazyParsley, LetFinderState, LetMap, RecMap}
import parsley.internal.deepembedding.frontend.debugger.Debugged

class RenameSpec extends AnyFlatSpec with Matchers {
  behavior of "the Renamer object"

  it should "not rename a parser it does not know of" in {
    val exampleParser = new DummyParser
    Rename(exampleParser) shouldNot be("exampleParser")
  }

  it should "rename a parser it is aware of" in {
    val exampleParser = new DummyParser
    Rename.addNames(Map(exampleParser -> "exampleParser"))

    Rename(exampleParser) shouldBe "exampleParser"
  }

  it should "translate a symbolic parser's name" in {
    val symbolic = new <**>
    Rename(symbolic) shouldBe "<**>"
  }

  it should "pass through Debugged parsers and get the inner parser's name" in {
    val symbolic = new <**>
    val debugged = new Debugged(symbolic, Some(symbolic))(new DebugContext())

    Rename(debugged) shouldBe "<**>"
  }
}

// These are dummy parsers used for the above tests.
// We don't actually care that they don't implement anything.
private class DummyParser extends LazyParsley[Any] {
  override protected def findLetsAux[Cont[_, +_] : ContOps, R]
  (seen: Set[LazyParsley[_]])(implicit state: LetFinderState): Cont[R, Unit] =
    ???

  override protected def preprocess[Cont[_, +_] : ContOps, R, A_ >: Any]
  (implicit lets: LetMap, recs: RecMap): Cont[R, StrictParsley[A_]] =
    ???
}

private class <**> extends LazyParsley[Any] {
  override protected def findLetsAux[Cont[_, +_] : ContOps, R]
  (seen: Set[LazyParsley[_]])(implicit state: LetFinderState): Cont[R, Unit] =
    ???

  override protected def preprocess[Cont[_, +_] : ContOps, R, A_ >: Any]
  (implicit lets: LetMap, recs: RecMap): Cont[R, StrictParsley[A_]] =
    ???
}

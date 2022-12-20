/* SPDX-FileCopyrightText: © 2021 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.errors

import parsley.ParsleyTest

import parsley.internal.errors.ExpectDesc
import scala.language.implicitConversions

import MockedBuilders.mockedErrorItemBuilder
import scala.annotation.nowarn

class DefuncHintsTests extends ParsleyTest {
    def mkErr(labels: String*): DefuncError = {
        assert(labels.nonEmpty)
        new MultiExpectedError(0, 0, 0, labels.map(ExpectDesc(_)).toSet, 1)
    }

    "EmptyHints" should "have size 0" in {
        EmptyHints shouldBe empty
    }
    it should "yield an empty set" in {
        EmptyHints.toSet shouldBe empty
    }

    "AddError" should "should increase the size" in {
        EmptyHints.addError(mkErr("a")) should not be empty
    }

    "ReplaceHint" should "do nothing on empty" in {
        EmptyHints.rename("hi") shouldBe empty
    }
    it should "replace the hints under it" in {
        val hints = EmptyHints.addError(mkErr("a", "c")).addError(mkErr("b")).rename("hi")
        hints.toSet should contain only (ExpectDesc("hi"))
    }

    "MergeHints" should "ensure all elements from both hints" in {
        val hints1 = EmptyHints.addError(mkErr("a")).addError(mkErr("b"))
        val hints2 = EmptyHints.addError(mkErr("c")).addError(mkErr("d"))
        hints1.merge(hints2).toSet should contain only (ExpectDesc("a"), ExpectDesc("b"), ExpectDesc("c"), ExpectDesc("d"))
    }
}

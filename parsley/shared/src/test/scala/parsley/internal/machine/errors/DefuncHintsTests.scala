/* SPDX-FileCopyrightText: © 2021 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.errors

import parsley.ParsleyTest

import parsley.internal.errors.ExpectDesc

class DefuncHintsTests extends ParsleyTest {
    def mkErr(labels: String*): DefuncError = {
        assert(labels.nonEmpty)
        new ExpectedError(0, 0, 0, labels.map(new ExpectDesc(_)), 1)
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
        EmptyHints.rename(Seq("hi")) shouldBe empty
    }
    it should "replace the hints under it" in {
        val hints = EmptyHints.addError(mkErr("a", "c")).addError(mkErr("b")).rename(Seq("hi"))
        hints.toSet should contain only (new ExpectDesc("hi"))
    }

    "MergeHints" should "ensure all elements from both hints" in {
        val hints1 = EmptyHints.addError(mkErr("a")).addError(mkErr("b"))
        val hints2 = EmptyHints.addError(mkErr("c")).addError(mkErr("d"))
        hints1.merge(hints2).toSet should contain.only(new ExpectDesc("a"), new ExpectDesc("b"), new ExpectDesc("c"), new ExpectDesc("d"))
    }
}

/* SPDX-FileCopyrightText: © 2021 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.errors

import parsley.ParsleyTest

import parsley.internal.errors.Desc
import scala.language.implicitConversions

import MockedBuilders.mockedErrorItemBuilder
import scala.annotation.nowarn

class DefuncHintsTests extends ParsleyTest {
    def mkErr(labels: String*): DefuncError = {
        assert(labels.nonEmpty)
        new MultiExpectedError(0, 0, 0, labels.map(Desc(_)).toSet, 1)
    }

    "EmptyHints" should "have size 0" in {
        EmptyHints shouldBe empty
    }
    it should "yield an empty set" in {
        EmptyHints.toSet shouldBe empty
    }

    "AddError" should "should increase the size" in {
        EmptyHints.addError(mkErr("a")) should have size 1
    }

    "PopHints" should "have minimum size 0" in {
        EmptyHints.pop should have size 0
    }
    it should "otherwise ensure it is smaller than before" in {
        val hints = EmptyHints.addError(mkErr("a")).addError(mkErr("b"))
        val hints_ = hints.pop
        hints should have size 2
        hints_ should have size 1
        hints_.toSet should contain only (Desc("b"))
        hints_.pop shouldBe empty
    }

    "ReplaceHint" should "do nothing on empty" in {
        EmptyHints.rename("hi") shouldBe empty
    }
    it should "replace the first set otherwise" in {
        val hints = EmptyHints.addError(mkErr("a", "c")).addError(mkErr("b")).rename("hi")
        hints.toSet should contain only (Desc("hi"), Desc("b"))
    }

    "MergeHints" should "ensure all elements from both hints" in {
        val hints1 = EmptyHints.addError(mkErr("a")).addError(mkErr("b"))
        val hints2 = EmptyHints.addError(mkErr("c")).addError(mkErr("d"))
        hints1.merge(hints2).toSet should contain only (Desc("a"), Desc("b"), Desc("c"), Desc("d"))
    }
    it should "ensure pops on the right do not impact the left" in {
        val hints1 = EmptyHints.addError(mkErr("a")).addError(mkErr("b"))
        val hints2 = EmptyHints.addError(mkErr("c")).addError(mkErr("d")).pop
        hints1.merge(hints2).toSet should contain only (Desc("a"), Desc("b"), Desc("d"))
    }
    it should "ensure that if the left needs complete popping that is ok" in {
        def hints1 = EmptyHints.addError(mkErr("a")).addError(mkErr("b"))
        def hints2 = EmptyHints.addError(mkErr("c")).addError(mkErr("d"))
        hints1.merge(hints2).pop.pop.pop.toSet should contain only (Desc("d"))
        hints1.pop.merge(hints2).pop.pop.toSet should contain only (Desc("d"))
    }
}

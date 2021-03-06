package parsley.internal.machine.errors

import parsley.ParsleyTest

import parsley.internal.errors.Desc
import scala.language.implicitConversions

import MockedBuilders.mockedErrorItemBuilder
import scala.annotation.nowarn

class DefuncHintsTests extends ParsleyTest {
    def mkErr(labels: String*): DefuncError = {
        assert(labels.nonEmpty)
        MultiExpectedError(0, 0, 0, labels.map(Desc(_)).toSet, 1)
    }

    "EmptyHints" should "have size 0" in {
        EmptyHints shouldBe empty
    }
    it should "yield an empty set" in {
        EmptyHints.toSet shouldBe empty
    }

    "AddError" should "should increase the size" in {
        AddError(EmptyHints, mkErr("a")) should have size 1
    }

    "PopHints" should "have minimum size 0" in {
        PopHints(EmptyHints) should have size 0
    }
    it should "otherwise ensure it is smaller than before" in {
        val hints = AddError(AddError(EmptyHints, mkErr("a")), mkErr("b"))
        val hints_ = PopHints(hints)
        hints should have size 2
        hints_ should have size 1
        hints_.toSet should contain only (Desc("b"))
        PopHints(hints_) shouldBe empty
    }

    "ReplaceHint" should "do nothing on empty" in {
        ReplaceHint("hi", EmptyHints) shouldBe empty
    }
    it should "replace the first set otherwise" in {
        val hints = ReplaceHint("hi", AddError(AddError(EmptyHints, mkErr("a", "c")), mkErr("b")))
        hints.toSet should contain only (Desc("hi"), Desc("b"))
    }

    "MergeHints" should "ensure all elements from both hints" in {
        val hints1 = AddError(AddError(EmptyHints, mkErr("a")), mkErr("b"))
        val hints2 = AddError(AddError(EmptyHints, mkErr("c")), mkErr("d"))
        MergeHints(hints1, hints2).toSet should contain only (Desc("a"), Desc("b"), Desc("c"), Desc("d"))
    }
    it should "ensure pops on the right do not impact the left" in {
        val hints1 = AddError(AddError(EmptyHints, mkErr("a")), mkErr("b"))
        val hints2 = PopHints(AddError(AddError(EmptyHints, mkErr("c")), mkErr("d")))
        MergeHints(hints1, hints2).toSet should contain only (Desc("a"), Desc("b"), Desc("d"))
    }
    it should "ensure that if the left needs complete popping that is ok" in {
        def hints1 = AddError(AddError(EmptyHints, mkErr("a")), mkErr("b"))
        def hints2 = AddError(AddError(EmptyHints, mkErr("c")), mkErr("d"))
        PopHints(PopHints(PopHints(MergeHints(hints1, hints2)))).toSet should contain only (Desc("d"))
        PopHints(PopHints(MergeHints(PopHints(hints1), hints2))).toSet should contain only (Desc("d"))
    }
}
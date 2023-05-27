/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.errors

import parsley.ParsleyTest

import parsley.internal.errors.{TrivialError, FancyError, FlexibleCaret, ExpectRaw, ExpectDesc, EndOfInput, RigidCaret, UnexpectDesc}

import MockedBuilders.mockedErrorItemBuilder

class DefuncErrorTests extends ParsleyTest {
    "ClassicExpectedError" should "evaluate to TrivialError" in {
        val err = new ClassicExpectedError(0, 0, 0, None, 1)
        err.isTrivialError shouldBe true
        err.asParseError shouldBe a [TrivialError]
    }
    it should "only be empty when its label is" in {
        new ClassicExpectedError(0, 0, 0, None, 1).isExpectedEmpty shouldBe true
        new ClassicExpectedError(0, 0, 0, Some(EndOfInput), 1).isExpectedEmpty shouldBe false
    }

    "ClassicExpectedErrorWithReason" should "evaluate to TrivialError" in {
        val err = new ClassicExpectedErrorWithReason(0, 0, 0, None, "", 1)
        err.isTrivialError shouldBe true
        err.asParseError shouldBe a [TrivialError]
    }
    it should "only be empty when its label is" in {
        new ClassicExpectedErrorWithReason(0, 0, 0, None, "", 1).isExpectedEmpty shouldBe true
        new ClassicExpectedErrorWithReason(0, 0, 0, Some(EndOfInput), "", 1).isExpectedEmpty shouldBe false
    }

    "ClassicUnexpectedError" should "evaluate to TrivialError" in {
        val err = new ClassicUnexpectedError(0, 0, 0, None, new UnexpectDesc("oops", new RigidCaret(1)))
        err.isTrivialError shouldBe true
        err.asParseError shouldBe a [TrivialError]
    }
    it should "only be empty when its label is" in {
        new ClassicUnexpectedError(0, 0, 0, None, new UnexpectDesc("oops", new RigidCaret(1))).isExpectedEmpty shouldBe true
        new ClassicUnexpectedError(0, 0, 0, Some(new ExpectDesc("oops")), new UnexpectDesc("oops", new RigidCaret(1))).isExpectedEmpty shouldBe false
    }
    it should "allow for flexible and rigid carets" in {
        val err = new ClassicExpectedError(0, 0, 0, None, 5)
        val errRigid = new ClassicUnexpectedError(0, 0, 0, None, new UnexpectDesc("oops", new RigidCaret(1)))
        val errFlex1 = new ClassicUnexpectedError(0, 0, 0, None, new UnexpectDesc("oops", new FlexibleCaret(1)))
        val errFlex2 = new ClassicUnexpectedError(0, 0, 0, None, new UnexpectDesc("oops", new FlexibleCaret(6)))
        val pRigid = errRigid.merge(err).asParseError
        pRigid shouldBe a [TrivialError]
        pRigid.asInstanceOf[TrivialError].unexpected.fold(identity, _.formatUnexpect(false)._2.toCaretLength(0, 10, Nil)) shouldBe 1
        val pFlex1 = errFlex1.merge(err).asParseError
        pFlex1 shouldBe a [TrivialError]
        pFlex1.asInstanceOf[TrivialError].unexpected.fold(identity, _.formatUnexpect(false)._2.toCaretLength(0, 10, Nil)) shouldBe 1 // FIXME:
        val pFlex2 = errFlex2.merge(err).asParseError
        pFlex2 shouldBe a [TrivialError]
        pFlex2.asInstanceOf[TrivialError].unexpected.fold(identity, _.formatUnexpect(false)._2.toCaretLength(0, 10, Nil)) shouldBe 6
    }

    "ClassicFancyError" should "evaluate to FancyError" in {
        val err = new ClassicFancyError(0, 0, 0, new RigidCaret(1), "")
        err.isTrivialError shouldBe false
        err.asParseError shouldBe a [FancyError]
    }
    it should "always be empty" in {
        new ClassicFancyError(0, 0, 0, new RigidCaret(1), "hi").isExpectedEmpty shouldBe true
    }

    "EmptyError" should "evaluate to TrivialError" in {
        val err = new EmptyError(0, 0, 0, 0)
        err.isTrivialError shouldBe true
        err.asParseError shouldBe a [TrivialError]
    }

    "EmptyErrorWithReason" should "evaluate to TrivialError" in {
        val err = new EmptyErrorWithReason(0, 0, 0, "", 0)
        err.isTrivialError shouldBe true
        err.asParseError shouldBe a [TrivialError]
    }

    "MultiExpectedError" should "evaluate to TrivialError" in {
        val err = new MultiExpectedError(0, 0, 0, Set.empty, 1)
        err.isTrivialError shouldBe true
        err.asParseError shouldBe a [TrivialError]
    }
    it should "only be empty when its label is" in {
        new MultiExpectedError(0, 0, 0, Set.empty, 1).isExpectedEmpty shouldBe true
        new MultiExpectedError(0, 0, 0, Set(EndOfInput), 1).isExpectedEmpty shouldBe false
    }

    "MergedErrors" should "be trivial if both children are" in {
        val err = new EmptyError(0, 0, 0, 0).merge(new MultiExpectedError(0, 0, 0, Set.empty, 1))
        err.isTrivialError shouldBe true
        err.asParseError shouldBe a [TrivialError]
    }
    they should "be a trivial error if one trivial child is further than the other fancy child" in {
        val err1 = new EmptyError(1, 0, 0, 0).merge(new ClassicFancyError(0, 0, 0, new RigidCaret(1), ""))
        err1.isTrivialError shouldBe true
        err1.asParseError shouldBe a [TrivialError]

        val err2 = new ClassicFancyError(0, 0, 0, new RigidCaret(1), "").merge(new EmptyError(1, 0, 0, 0))
        err2.isTrivialError shouldBe true
        err2.asParseError shouldBe a [TrivialError]
    }
    they should "be a fancy error in any other case" in {
        val err1 = new EmptyError(0, 0, 0, 4).merge(new ClassicFancyError(0, 0, 0, new RigidCaret(1), ""))
        err1.isTrivialError shouldBe false
        err1.flexibleCaret shouldBe false
        val perr1 = err1.asParseError
        perr1 shouldBe a [FancyError]
        perr1.asInstanceOf[FancyError].caretWidth shouldBe 1

        val err2 = new ClassicFancyError(0, 0, 0, new FlexibleCaret(1), "").merge(new EmptyError(0, 0, 0, 4))
        err2.isTrivialError shouldBe false
        err2.flexibleCaret shouldBe true
        val perr2 = err2.asParseError
        perr2 shouldBe a [FancyError]
        perr2.asInstanceOf[FancyError].caretWidth shouldBe 4

        val err3 = new EmptyError(0, 0, 0, 0).merge(new ClassicFancyError(1, 0, 0, new RigidCaret(1), ""))
        err3.isTrivialError shouldBe false
        err3.asParseError shouldBe a [FancyError]

        val err4 = new ClassicFancyError(1, 0, 0, new RigidCaret(1), "").merge(new EmptyError(0, 0, 0, 0))
        err4.isTrivialError shouldBe false
        err4.asParseError shouldBe a [FancyError]

        val err5 = new ClassicFancyError(0, 0, 0, new FlexibleCaret(3), "").merge(new ClassicFancyError(0, 0, 0, new RigidCaret(1), ""))
        err5.isTrivialError shouldBe false
        err5.flexibleCaret shouldBe false
        val perr5 = err5.asParseError
        perr5 shouldBe a [FancyError]
        perr5.asInstanceOf[FancyError].caretWidth shouldBe 1

        val err6 = new ClassicFancyError(1, 0, 0, new RigidCaret(1), "").merge(new ClassicFancyError(0, 0, 0, new RigidCaret(1), ""))
        err6.isTrivialError shouldBe false
        err6.asParseError shouldBe a [FancyError]
    }
    they should "be empty when trivial and same offset only when both children are empty" in {
        new EmptyError(0, 0, 0, 0).merge(new EmptyError(0, 0, 0, 0)).isExpectedEmpty shouldBe true
        new EmptyError(0, 0, 0, 0).merge(new ClassicExpectedError(0, 0, 0, Some(EndOfInput), 1)).isExpectedEmpty shouldBe false
        new ClassicExpectedError(0, 0, 0, Some(EndOfInput), 1).merge(new EmptyError(0, 0, 0, 0)).isExpectedEmpty shouldBe false
        new ClassicExpectedError(0, 0, 0, Some(EndOfInput), 1).merge(new ClassicExpectedError(0, 0, 0, Some(EndOfInput), 1)).isExpectedEmpty shouldBe false
    }
    they should "contain all the expecteds from both branches when appropriate" in {
        val err = new MultiExpectedError(0, 0, 0, Set(new ExpectRaw("a"), new ExpectRaw("b")), 1).merge(new MultiExpectedError(0, 0, 0, Set(new ExpectRaw("b"), new ExpectRaw("c")), 1))
        err.asParseError.asInstanceOf[TrivialError].expecteds should contain only (new ExpectRaw("a"), new ExpectRaw("b"), new ExpectRaw("c"))
    }

    "WithHints" should "be trivial if its child is" in {
        val err = new ClassicExpectedError(0, 0, 0, None, 1).withHints(EmptyHints)
        err.isTrivialError shouldBe true
        err.asParseError shouldBe a [TrivialError]
    }
    it should "support fancy errors as not trivial" in {
        val err = new ClassicFancyError(0, 0, 0, new RigidCaret(1), "").withHints(EmptyHints)
        err.isTrivialError shouldBe false
        err.asParseError shouldBe a [FancyError]
    }
    it should "only be empty when its label is" in {
        new EmptyError(0, 0, 0, 0).withHints(EmptyHints).isExpectedEmpty shouldBe true
        new ClassicExpectedError(0, 0, 0, Some(EndOfInput), 1).withHints(EmptyHints).isExpectedEmpty shouldBe false
    }

    "WithReason" should "be trivial if its child is" in {
        val err = new ClassicExpectedError(0, 0, 0, None, 1).withReason("")
        err.isTrivialError shouldBe true
        err.asParseError shouldBe a [TrivialError]
    }
    it should "support fancy errors as not trivial" in {
        val err = new ClassicFancyError(0, 0, 0, new RigidCaret(1), "").withReason("")
        err.isTrivialError shouldBe false
        err.asParseError shouldBe a [FancyError]
    }
    it should "only be empty when its label is" in {
        new EmptyError(0, 0, 0, 0).withReason("").isExpectedEmpty shouldBe true
        new ClassicExpectedError(0, 0, 0, Some(EndOfInput), 1).withReason("").isExpectedEmpty shouldBe false
    }

    "WithLabel" should "be trivial if its child is" in {
        val err = new ClassicExpectedError(0, 0, 0, None, 1).label("", 0)
        err.isTrivialError shouldBe true
        err.asParseError shouldBe a [TrivialError]
    }
    it should "support fancy errors as not trivial" in {
        val err = new ClassicFancyError(0, 0, 0, new RigidCaret(1), "").label("", 0)
        err.isTrivialError shouldBe false
        err.asParseError shouldBe a [FancyError]
    }
    it should "be empty if the label is empty and not otherwise" in {
        new EmptyError(0, 0, 0, 0).label("", 0).isExpectedEmpty shouldBe true
        new EmptyError(0, 0, 0, 0).label("a", 0).isExpectedEmpty shouldBe false
        new ClassicExpectedError(0, 0, 0, Some(new ExpectDesc("x")), 1).label("", 0).isExpectedEmpty shouldBe true
        new ClassicExpectedError(0, 0, 0, Some(new ExpectDesc("x")), 1).label("a", 0).isExpectedEmpty shouldBe false
    }
    it should "replace all expected" in {
        val errShow = new MultiExpectedError(0, 0, 0, Set(new ExpectRaw("a"), new ExpectRaw("b")), 1).label("x", 0)
        val errHide = new MultiExpectedError(0, 0, 0, Set(new ExpectRaw("a"), new ExpectRaw("b")), 1).label("", 0)
        errShow.asParseError.expecteds should contain only (new ExpectDesc("x"))
        errHide.asParseError.expecteds shouldBe empty
    }

    "Amended" should "Change the error position information" in {
        val err =  new EmptyError(0, 0, 0, 0).amend(10, 10, 10)
        val errOut = err.asParseError
        errOut.col shouldBe 10
        errOut.line shouldBe 10
        errOut.offset shouldBe 10
    }
    it should "work for fancy errors too" in {
        val err = new ClassicFancyError(0, 0, 0, new RigidCaret(1), "").amend(10, 10, 10)
        val errOut = err.asParseError
        errOut.col shouldBe 10
        errOut.line shouldBe 10
        errOut.offset shouldBe 10
    }

    "Entrenched" should "guard against amendment" in {
        val err = new EmptyError(0, 0, 0, 0).entrench.amend(10, 10, 10)
        val errOut = err.asParseError
        err.entrenched shouldBe true
        errOut.col shouldBe 0
        errOut.line shouldBe 0
        errOut.offset shouldBe 0
    }
    it should "work for fancy errors too" in {
        val err = new ClassicFancyError(0, 0, 0, new RigidCaret(1), "").entrench.amend(10, 10, 10)
        val errOut = err.asParseError
        err.entrenched shouldBe true
        errOut.col shouldBe 0
        errOut.line shouldBe 0
        errOut.offset shouldBe 0
    }

    "Dislodged" should "remove an entrenchment" in {
        val err = new EmptyError(0, 0, 0, 0).entrench
        require(err.entrenched)
        err.dislodge.entrenched shouldBe false
        val err2 = err.dislodge.amend(10, 10, 10).asParseError
        err2.col shouldBe 10
        err2.line shouldBe 10
        err2.offset shouldBe 10
    }
    it should "work for fancy errors too" in {
        val err = new ClassicFancyError(0, 0, 0, new RigidCaret(1), "").entrench
        require(err.entrenched)
        err.dislodge.entrenched shouldBe false
        val err2 = err.dislodge.amend(10, 10, 10).asParseError
        err2.col shouldBe 10
        err2.line shouldBe 10
        err2.offset shouldBe 10
    }
}

/* SPDX-FileCopyrightText: © 2021 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.errors

import parsley.ParsleyTest

import parsley.internal.errors.{TrivialError, FancyError, Raw, Desc, EndOfInput}
import scala.language.implicitConversions

import MockedBuilders.mockedErrorItemBuilder

class DefuncErrorTests extends ParsleyTest {
    "ClassicExpectedError" should "evaluate to TrivialError" in {
        val err = ClassicExpectedError(0, 0, 0, None)
        err should be a trivialError
        err.asParseError shouldBe a [TrivialError]
    }
    it should "only be empty when its label is" in {
        ClassicExpectedError(0, 0, 0, None) shouldBe expectedEmpty
        ClassicExpectedError(0, 0, 0, Some(EndOfInput)) should not be expectedEmpty
    }

    "ClassicExpectedErrorWithReason" should "evaluate to TrivialError" in {
        val err = ClassicExpectedErrorWithReason(0, 0, 0, None, "")
        err should be a trivialError
        err.asParseError shouldBe a [TrivialError]
    }
    it should "only be empty when its label is" in {
        ClassicExpectedErrorWithReason(0, 0, 0, None, "") shouldBe expectedEmpty
        ClassicExpectedErrorWithReason(0, 0, 0, Some(EndOfInput), "") should not be expectedEmpty
    }

    "ClassicUnexpectedError" should "evaluate to TrivialError" in {
        val err = ClassicUnexpectedError(0, 0, 0, None, EndOfInput)
        err should be a trivialError
        err.asParseError shouldBe a [TrivialError]
    }
    it should "only be empty when its label is" in {
        ClassicUnexpectedError(0, 0, 0, None, EndOfInput) shouldBe expectedEmpty
        ClassicUnexpectedError(0, 0, 0, Some(EndOfInput), EndOfInput) should not be expectedEmpty
    }

    "ClassicFancyError" should "evaluate to FancyError" in {
        val err = ClassicFancyError(0, 0, 0, "")
        err shouldNot be a trivialError
        err.asParseError shouldBe a [FancyError]
    }
    it should "always be empty" in {
        ClassicFancyError(0, 0, 0, "hi") shouldBe expectedEmpty
    }

    "EmptyError" should "evaluate to TrivialError" in {
        val err = EmptyError(0, 0, 0)
        err should be a trivialError
        err.asParseError shouldBe a [TrivialError]
    }

    "TokenError" should "evaluate to TrivialError" in {
        val err = TokenError(0, 0, 0, None, 1)
        err should be a trivialError
        err.asParseError shouldBe a [TrivialError]
    }
    it should "only be empty when its label is" in {
        TokenError(0, 0, 0, None, 1) shouldBe expectedEmpty
        TokenError(0, 0, 0, Some(EndOfInput), 1) should not be expectedEmpty
    }

    "EmptyErrorWithReason" should "evaluate to TrivialError" in {
        val err = EmptyErrorWithReason(0, 0, 0, "")
        err should be a trivialError
        err.asParseError shouldBe a [TrivialError]
    }

    "MultiExpectedError" should "evaluate to TrivialError" in {
        val err = MultiExpectedError(0, 0, 0, Set.empty, 1)
        err should be a trivialError
        err.asParseError shouldBe a [TrivialError]
    }
    it should "only be empty when its label is" in {
        MultiExpectedError(0, 0, 0, Set.empty, 1) shouldBe expectedEmpty
        MultiExpectedError(0, 0, 0, Set(EndOfInput), 1) should not be expectedEmpty
    }

    "MergedErrors" should "be trivial if both children are" in {
        val err = MergedErrors(EmptyError(0, 0, 0), MultiExpectedError(0, 0, 0, Set.empty, 1))
        err should be a trivialError
        err.asParseError shouldBe a [TrivialError]
    }
    they should "be a trivial error if one trivial child is further than the other fancy child" in {
        val err1 = MergedErrors(EmptyError(1, 0, 0), ClassicFancyError(0, 0, 0, ""))
        err1 should be a trivialError
        err1.asParseError shouldBe a [TrivialError]

        val err2 = MergedErrors(ClassicFancyError(0, 0, 0, ""), EmptyError(1, 0, 0))
        err2 should be a trivialError
        err2.asParseError shouldBe a [TrivialError]
    }
    they should "be a fancy error in any other case" in {
        val err1 = MergedErrors(EmptyError(0, 0, 0), ClassicFancyError(0, 0, 0, ""))
        err1 shouldNot be a trivialError
        err1.asParseError shouldBe a [FancyError]

        val err2 = MergedErrors(ClassicFancyError(0, 0, 0, ""), EmptyError(0, 0, 0))
        err2 shouldNot be a trivialError
        err2.asParseError shouldBe a [FancyError]

        val err3 = MergedErrors(EmptyError(0, 0, 0), ClassicFancyError(1, 0, 0, ""))
        err3 shouldNot be a trivialError
        err3.asParseError shouldBe a [FancyError]

        val err4 = MergedErrors(ClassicFancyError(1, 0, 0, ""), EmptyError(0, 0, 0))
        err4 shouldNot be a trivialError
        err4.asParseError shouldBe a [FancyError]

        val err5 = MergedErrors(ClassicFancyError(0, 0, 0, ""), ClassicFancyError(0, 0, 0, ""))
        err5 shouldNot be a trivialError
        err5.asParseError shouldBe a [FancyError]

        val err6 = MergedErrors(ClassicFancyError(1, 0, 0, ""), ClassicFancyError(0, 0, 0, ""))
        err6 shouldNot be a trivialError
        err6.asParseError shouldBe a [FancyError]
    }
    they should "be empty when trivial and same offset only when both children are empty" in {
        MergedErrors(EmptyError(0, 0, 0), EmptyError(0, 0, 0)) shouldBe expectedEmpty
        MergedErrors(EmptyError(0, 0, 0), ClassicExpectedError(0, 0, 0, Some(EndOfInput))) should not be expectedEmpty
        MergedErrors(ClassicExpectedError(0, 0, 0, Some(EndOfInput)), EmptyError(0, 0, 0)) should not be expectedEmpty
        MergedErrors(ClassicExpectedError(0, 0, 0, Some(EndOfInput)), ClassicExpectedError(0, 0, 0, Some(EndOfInput))) should not be expectedEmpty
    }
    they should "contain all the expecteds from both branches when appropriate" in {
        val err = MergedErrors(MultiExpectedError(0, 0, 0, Set(Raw("a"), Raw("b")), 1),
                               MultiExpectedError(0, 0, 0, Set(Raw("b"), Raw("c")), 1))
        err.asParseError.asInstanceOf[TrivialError].expecteds should contain only (Raw("a"), Raw("b"), Raw("c"))
    }

    "WithHints" should "be trivial if its child is" in {
        val err = WithHints(ClassicExpectedError(0, 0, 0, None), EmptyHints)
        err should be a trivialError
        err.asParseError shouldBe a [TrivialError]
    }
    it should "support fancy errors as not trivial" in {
        val err = WithHints(ClassicFancyError(0, 0, 0, ""), EmptyHints)
        err shouldNot be a trivialError
        err.asParseError shouldBe a [FancyError]
    }
    it should "only be empty when its label is" in {
        WithHints(EmptyError(0, 0, 0), EmptyHints) shouldBe expectedEmpty
        WithHints(ClassicExpectedError(0, 0, 0, Some(EndOfInput)), EmptyHints) should not be expectedEmpty
    }

    "WithReason" should "be trivial if its child is" in {
        val err = WithReason(ClassicExpectedError(0, 0, 0, None), "")
        err should be a trivialError
        err.asParseError shouldBe a [TrivialError]
    }
    it should "support fancy errors as not trivial" in {
        val err = WithReason(ClassicFancyError(0, 0, 0, ""), "")
        err shouldNot be a trivialError
        err.asParseError shouldBe a [FancyError]
    }
    it should "only be empty when its label is" in {
        WithReason(EmptyError(0, 0, 0), "") shouldBe expectedEmpty
        WithReason(ClassicExpectedError(0, 0, 0, Some(EndOfInput)), "") should not be expectedEmpty
    }

    "WithLabel" should "be trivial if its child is" in {
        val err = WithLabel(ClassicExpectedError(0, 0, 0, None), "")
        err should be a trivialError
        err.asParseError shouldBe a [TrivialError]
    }
    it should "support fancy errors as not trivial" in {
        val err = WithLabel(ClassicFancyError(0, 0, 0, ""), "")
        err shouldNot be a trivialError
        err.asParseError shouldBe a [FancyError]
    }
    it should "be empty if the label is empty and not otherwise" in {
        WithLabel(EmptyError(0, 0, 0), "") shouldBe expectedEmpty
        WithLabel(EmptyError(0, 0, 0), "a") should not be expectedEmpty
        WithLabel(ClassicExpectedError(0, 0, 0, Some(Desc("x"))), "") shouldBe expectedEmpty
        WithLabel(ClassicExpectedError(0, 0, 0, Some(Desc("x"))), "a") should not be expectedEmpty
    }
    it should "replace all expected" in {
        val errShow = WithLabel(MultiExpectedError(0, 0, 0, Set(Raw("a"), Raw("b")), 1), "x")
        val errHide = WithLabel(MultiExpectedError(0, 0, 0, Set(Raw("a"), Raw("b")), 1), "")
        errShow.asParseError.asInstanceOf[TrivialError].expecteds should contain only (Desc("x"))
        errHide.asParseError.asInstanceOf[TrivialError].expecteds shouldBe empty
    }

    "Amended" should "Change the error position information" in {
        val err = Amended(10, 10, 10, EmptyError(0, 0, 0))
        val errOut = err.asParseError
        errOut.col shouldBe 10
        errOut.line shouldBe 10
        errOut.offset shouldBe 10
    }
    it should "work for fancy errors too" in {
        val err = Amended(10, 10, 10, ClassicFancyError(0, 0, 0, ""))
        val errOut = err.asParseError
        errOut.col shouldBe 10
        errOut.line shouldBe 10
        errOut.offset shouldBe 10
    }

    "Entrenched" should "guard against amendment" in {
        val err = Amended(10, 10, 10, Entrenched(EmptyError(0, 0, 0)))
        val errOut = err.asParseError
        errOut.col shouldBe 0
        errOut.line shouldBe 0
        errOut.offset shouldBe 0
    }
    it should "work for fancy errors too" in {
        val err = Amended(10, 10, 10, Entrenched(ClassicFancyError(0, 0, 0, "")))
        val errOut = err.asParseError
        errOut.col shouldBe 0
        errOut.line shouldBe 0
        errOut.offset shouldBe 0
    }
}

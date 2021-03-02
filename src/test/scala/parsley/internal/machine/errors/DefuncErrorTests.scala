package parsley.internal.machine.errors

import parsley.ParsleyTest

import parsley.internal.errors.{TrivialError, FailError, Raw, Desc, EndOfInput}
import scala.language.implicitConversions

import MockedBuilders.mockedErrorItemBuilder
import scala.annotation.nowarn

@nowarn("msg=deprecated")
class DefuncErrorTests extends ParsleyTest {
    "ClassicExpectedError" should "evaluate to TrivialError" in {
        val err = ClassicExpectedError(0, 0, 0, None)
        err should be a 'trivialError
        err.asParseError shouldBe a [TrivialError]
    }

    "ClassicExpectedErrorWithReason" should "evaluate to TrivialError" in {
        val err = ClassicExpectedErrorWithReason(0, 0, 0, None, "")
        err should be a 'trivialError
        err.asParseError shouldBe a [TrivialError]
    }

    "ClassicUnexpectedError" should "evaluate to TrivialError" in {
        val err = ClassicUnexpectedError(0, 0, 0, None, EndOfInput)
        err should be a 'trivialError
        err.asParseError shouldBe a [TrivialError]
    }

    "ClassicFancyError" should "evaluate to FancyError" in {
        val err = ClassicFancyError(0, 0, 0, "")
        err shouldNot be a 'trivialError
        err.asParseError shouldBe a [FailError]
    }

    "EmptyError" should "evaluate to TrivialError" in {
        val err = EmptyError(0, 0, 0, None)
        err should be a 'trivialError
        err.asParseError shouldBe a [TrivialError]
    }

    "StringTokError" should "evaluate to TrivialError" in {
        val err = StringTokError(0, 0, 0, None, 1)
        err should be a 'trivialError
        err.asParseError shouldBe a [TrivialError]
    }

    "EmptyErrorWithReason" should "evaluate to TrivialError" in {
        val err = EmptyErrorWithReason(0, 0, 0, None, "")
        err should be a 'trivialError
        err.asParseError shouldBe a [TrivialError]
    }

    "MultiExpectedError" should "evaluate to TrivialError" in {
        val err = MultiExpectedError(0, 0, 0, Set.empty)
        err should be a 'trivialError
        err.asParseError shouldBe a [TrivialError]
    }

    "MergedErrors" should "be trivial if both children are" in {
        val err = MergedErrors(EmptyError(0, 0, 0, None), MultiExpectedError(0, 0, 0, Set.empty))
        err should be a 'trivialError
        err.asParseError shouldBe a [TrivialError]
    }
    they should "be a trivial error if one trivial child is further than the other fancy child" in {
        val err1 = MergedErrors(EmptyError(1, 0, 0, None), ClassicFancyError(0, 0, 0, ""))
        err1 should be a 'trivialError
        err1.asParseError shouldBe a [TrivialError]

        val err2 = MergedErrors(ClassicFancyError(0, 0, 0, ""), EmptyError(1, 0, 0, None))
        err2 should be a 'trivialError
        err2.asParseError shouldBe a [TrivialError]
    }
    they should "be a fancy error in any other case" in {
        val err1 = MergedErrors(EmptyError(0, 0, 0, None), ClassicFancyError(0, 0, 0, ""))
        err1 shouldNot be a 'trivialError
        err1.asParseError shouldBe a [FailError]

        val err2 = MergedErrors(ClassicFancyError(0, 0, 0, ""), EmptyError(0, 0, 0, None))
        err2 shouldNot be a 'trivialError
        err2.asParseError shouldBe a [FailError]

        val err3 = MergedErrors(EmptyError(0, 0, 0, None), ClassicFancyError(1, 0, 0, ""))
        err3 shouldNot be a 'trivialError
        err3.asParseError shouldBe a [FailError]

        val err4 = MergedErrors(ClassicFancyError(1, 0, 0, ""), EmptyError(0, 0, 0, None))
        err4 shouldNot be a 'trivialError
        err4.asParseError shouldBe a [FailError]

        val err5 = MergedErrors(ClassicFancyError(0, 0, 0, ""), ClassicFancyError(0, 0, 0, ""))
        err5 shouldNot be a 'trivialError
        err5.asParseError shouldBe a [FailError]

        val err6 = MergedErrors(ClassicFancyError(1, 0, 0, ""), ClassicFancyError(0, 0, 0, ""))
        err6 shouldNot be a 'trivialError
        err6.asParseError shouldBe a [FailError]
    }

    "WithHints" should "be trivial if its child is" in {
        val err = WithHints(ClassicExpectedError(0, 0, 0, None), EmptyHints)
        err should be a 'trivialError
        err.asParseError shouldBe a [TrivialError]
    }
    it should "support fancy errors as not trivial" in {
        val err = WithHints(ClassicFancyError(0, 0, 0, ""), EmptyHints)
        err shouldNot be a 'trivialError
        err.asParseError shouldBe a [FailError]
    }

    "WithReason" should "be trivial if its child is" in {
        val err = WithReason(ClassicExpectedError(0, 0, 0, None), "")
        err should be a 'trivialError
        err.asParseError shouldBe a [TrivialError]
    }
    it should "support fancy errors as not trivial" in {
        val err = WithReason(ClassicFancyError(0, 0, 0, ""), "")
        err shouldNot be a 'trivialError
        err.asParseError shouldBe a [FailError]
    }

    "WithLabel" should "be trivial if its child is" in {
        val err = WithLabel(ClassicExpectedError(0, 0, 0, None), "")
        err should be a 'trivialError
        err.asParseError shouldBe a [TrivialError]
    }
    it should "support fancy errors as not trivial" in {
        val err = WithLabel(ClassicFancyError(0, 0, 0, ""), "")
        err shouldNot be a 'trivialError
        err.asParseError shouldBe a [FailError]
    }

}
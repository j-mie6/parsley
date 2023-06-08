package parsley.errors

import parsley.{ParsleyTest, TestErrorBuilder}
import parsley.errors._
import parsley.errors.tokenextractors._

class TokenExtractorTests extends ParsleyTest {
    val singleChar = new TestErrorBuilder with SingleChar
    val matchParserDemand = new TestErrorBuilder with MatchParserDemand
    val tillNextWhitespaceTrimmed = new TestErrorBuilder with TillNextWhitespace {
        def trimToParserDemand: Boolean = true
    }
    val tillNextWhitespaceRaw = new TestErrorBuilder with TillNextWhitespace {
        def trimToParserDemand: Boolean = false
    }
    // TODO: lexToken

    "SingleChar" should "return a single printable ascii character" in {
        singleChar.unexpectedToken("abc", 1, false) shouldBe Token.Raw("a")
        singleChar.unexpectedToken("1", 1, false) shouldBe Token.Raw("1")
        singleChar.unexpectedToken(";", 2, true) shouldBe Token.Raw(";")
    }
    it should "handle supplementary unicode characters" in {
        singleChar.unexpectedToken("🙂", 1, true) shouldBe Token.Raw("🙂")
        singleChar.unexpectedToken("🙂🙂🙂", 1, false) shouldBe Token.Raw("🙂")
    }
    it should "deal with whitespace characters by naming them" in {
        singleChar.unexpectedToken(" ", 1, true) shouldBe Token.Named("space", TokenSpan.Width(1))
        singleChar.unexpectedToken("\n", 1, true) shouldBe Token.Named("newline", TokenSpan.Width(1))
        singleChar.unexpectedToken("\t", 1, true) shouldBe Token.Named("tab", TokenSpan.Width(1))
        singleChar.unexpectedToken("\r", 1, true) shouldBe Token.Named("carriage return", TokenSpan.Width(1))
        singleChar.unexpectedToken("\f", 1, true) shouldBe Token.Named("whitespace character", TokenSpan.Width(1))
    }
    it should "refuse to print control characters" in {
        singleChar.unexpectedToken("\u0000", 1, true) shouldBe Token.Named("non-printable character (\\u0000)", TokenSpan.Width(1))
        singleChar.unexpectedToken("\u0001", 1, true) shouldBe Token.Named("non-printable character (\\u0001)", TokenSpan.Width(1))
        singleChar.unexpectedToken("\ud83d", 1, true) shouldBe Token.Named("non-printable character (\\ud83d)", TokenSpan.Width(1))
    }
    it should "refuse to print non-printable supplementary characters" in {
        singleChar.unexpectedToken(Character.toChars(0x0f0000), 1, true) shouldBe Token.Named("non-printable codepoint (\\udb80\\udc00, or 0x0f0000)", TokenSpan.Width(1))
        singleChar.unexpectedToken(Character.toChars(0x10ffff), 1, true) shouldBe Token.Named("non-printable codepoint (\\udbff\\udfff, or 0x10ffff)", TokenSpan.Width(1))
    }
}

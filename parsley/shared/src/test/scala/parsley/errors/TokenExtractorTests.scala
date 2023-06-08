/* SPDX-FileCopyrightText: © 2023 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.errors

import parsley.{ParsleyTest, TestErrorBuilder}
import parsley.errors._
import parsley.errors.tokenextractors._

class TokenExtractorTests extends ParsleyTest {
    val singleChar = new TestErrorBuilder with SingleChar
    "SingleChar" should "return a single printable ascii character" in {
        singleChar.unexpectedToken("abc", 1, false) shouldBe Token.Raw("a")
        singleChar.unexpectedToken("1", 1, false) shouldBe Token.Raw("1")
        singleChar.unexpectedToken(";", 2, true) shouldBe Token.Raw(";")
    }
    it should "handle supplementary unicode characters" in {
        singleChar.unexpectedToken("😀", 1, true) shouldBe Token.Raw("😀")
        singleChar.unexpectedToken("😀😀😀", 1, false) shouldBe Token.Raw("😀")
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

    val matchParserDemand = new TestErrorBuilder with MatchParserDemand
    "MatchParserDemand" should "return a single printable ascii character" in {
        matchParserDemand.unexpectedToken("abc", 1, false) shouldBe Token.Raw("a")
        matchParserDemand.unexpectedToken("abc", 2, false) shouldBe Token.Raw("ab")
        matchParserDemand.unexpectedToken("1", 4, false) shouldBe Token.Raw("1")
        matchParserDemand.unexpectedToken(";", 2, true) shouldBe Token.Raw(";")
        matchParserDemand.unexpectedToken(";.,", 3, true) shouldBe Token.Raw(";.,")
    }
    it should "handle supplementary unicode characters" in {
        matchParserDemand.unexpectedToken("😀", 2, true) shouldBe Token.Raw("😀")
        matchParserDemand.unexpectedToken("😀😀😀", 2, false) shouldBe Token.Raw("😀😀")
    }
    it should "deal with whitespace characters by naming them" in {
        matchParserDemand.unexpectedToken(" aa", 2, true) shouldBe Token.Named("space", TokenSpan.Width(1))
        matchParserDemand.unexpectedToken("\naa", 2, true) shouldBe Token.Named("newline", TokenSpan.Width(1))
        matchParserDemand.unexpectedToken("\taa", 2, true) shouldBe Token.Named("tab", TokenSpan.Width(1))
        matchParserDemand.unexpectedToken("\raa", 3, true) shouldBe Token.Named("carriage return", TokenSpan.Width(1))
        matchParserDemand.unexpectedToken("\faa", 1, true) shouldBe Token.Named("whitespace character", TokenSpan.Width(1))
    }
    it should "refuse to print control characters" in {
        matchParserDemand.unexpectedToken("\u0000aaa", 3, true) shouldBe Token.Named("non-printable character (\\u0000)", TokenSpan.Width(1))
        matchParserDemand.unexpectedToken("\u0001aaa", 3, true) shouldBe Token.Named("non-printable character (\\u0001)", TokenSpan.Width(1))
        matchParserDemand.unexpectedToken("\ud83daaa", 2, true) shouldBe Token.Named("non-printable character (\\ud83d)", TokenSpan.Width(1))
    }
    it should "refuse to print non-printable supplementary characters" in {
        matchParserDemand.unexpectedToken(Character.toChars(0x0f0000), 1, true) shouldBe Token.Named("non-printable codepoint (\\udb80\\udc00, or 0x0f0000)", TokenSpan.Width(1))
        matchParserDemand.unexpectedToken(Character.toChars(0x10ffff), 1, true) shouldBe Token.Named("non-printable codepoint (\\udbff\\udfff, or 0x10ffff)", TokenSpan.Width(1))
    }

    val tillNextWhitespaceTrimmed = new TestErrorBuilder with TillNextWhitespace {
        def trimToParserDemand: Boolean = true
    }
    val tillNextWhitespaceRaw = new TestErrorBuilder with TillNextWhitespace {
        def trimToParserDemand: Boolean = false
    }
    "TillNextWhitespace" should "return a single printable ascii character" in {
        tillNextWhitespaceTrimmed.unexpectedToken("abc", 1, false) shouldBe Token.Raw("a")
        tillNextWhitespaceTrimmed.unexpectedToken("abc ", 2, false) shouldBe Token.Raw("ab")
        tillNextWhitespaceTrimmed.unexpectedToken("a  bc", 2, false) shouldBe Token.Raw("a")
        tillNextWhitespaceTrimmed.unexpectedToken("1\n23", 4, false) shouldBe Token.Raw("1")
        tillNextWhitespaceTrimmed.unexpectedToken("; ", 2, true) shouldBe Token.Raw(";")
        tillNextWhitespaceTrimmed.unexpectedToken(";. ,", 3, true) shouldBe Token.Raw(";.")
        tillNextWhitespaceRaw.unexpectedToken("abc", 1, false) shouldBe Token.Raw("abc")
        tillNextWhitespaceRaw.unexpectedToken("abc ", 2, false) shouldBe Token.Raw("abc")
        tillNextWhitespaceRaw.unexpectedToken("a  bc", 2, false) shouldBe Token.Raw("a")
        tillNextWhitespaceRaw.unexpectedToken("1\n23", 4, false) shouldBe Token.Raw("1")
        tillNextWhitespaceRaw.unexpectedToken("; ", 2, true) shouldBe Token.Raw(";")
        tillNextWhitespaceRaw.unexpectedToken(";. ,", 3, true) shouldBe Token.Raw(";.")
    }
    it should "handle supplementary unicode characters" in {
        tillNextWhitespaceTrimmed.unexpectedToken("😀", 2, true) shouldBe Token.Raw("😀")
        tillNextWhitespaceTrimmed.unexpectedToken("😀😀😀 😀", 2, false) shouldBe Token.Raw("😀😀")
        tillNextWhitespaceRaw.unexpectedToken("😀 😀", 2, true) shouldBe Token.Raw("😀")
        tillNextWhitespaceRaw.unexpectedToken("😀😀 😀", 1, false) shouldBe Token.Raw("😀😀")
    }
    it should "deal with whitespace characters by naming them" in {
        tillNextWhitespaceTrimmed.unexpectedToken(" aa", 2, true) shouldBe Token.Named("space", TokenSpan.Width(1))
        tillNextWhitespaceTrimmed.unexpectedToken("\naa", 2, true) shouldBe Token.Named("newline", TokenSpan.Width(1))
        tillNextWhitespaceTrimmed.unexpectedToken("\taa", 2, true) shouldBe Token.Named("tab", TokenSpan.Width(1))
        tillNextWhitespaceTrimmed.unexpectedToken("\raa", 3, true) shouldBe Token.Named("carriage return", TokenSpan.Width(1))
        tillNextWhitespaceTrimmed.unexpectedToken("\faa", 1, true) shouldBe Token.Named("whitespace character", TokenSpan.Width(1))
        tillNextWhitespaceRaw.unexpectedToken(" aa", 2, true) shouldBe Token.Named("space", TokenSpan.Width(1))
        tillNextWhitespaceRaw.unexpectedToken("\naa", 2, true) shouldBe Token.Named("newline", TokenSpan.Width(1))
        tillNextWhitespaceRaw.unexpectedToken("\taa", 2, true) shouldBe Token.Named("tab", TokenSpan.Width(1))
        tillNextWhitespaceRaw.unexpectedToken("\raa", 3, true) shouldBe Token.Named("carriage return", TokenSpan.Width(1))
        tillNextWhitespaceRaw.unexpectedToken("\faa", 1, true) shouldBe Token.Named("whitespace character", TokenSpan.Width(1))
    }
    it should "refuse to print control characters" in {
        tillNextWhitespaceTrimmed.unexpectedToken("\u0000aaa", 3, true) shouldBe Token.Named("non-printable character (\\u0000)", TokenSpan.Width(1))
        tillNextWhitespaceTrimmed.unexpectedToken("\u0001aaa", 3, true) shouldBe Token.Named("non-printable character (\\u0001)", TokenSpan.Width(1))
        tillNextWhitespaceTrimmed.unexpectedToken("\ud83daaa", 2, true) shouldBe Token.Named("non-printable character (\\ud83d)", TokenSpan.Width(1))
        tillNextWhitespaceRaw.unexpectedToken("\u0000aaa", 3, true) shouldBe Token.Named("non-printable character (\\u0000)", TokenSpan.Width(1))
        tillNextWhitespaceRaw.unexpectedToken("\u0001aaa", 3, true) shouldBe Token.Named("non-printable character (\\u0001)", TokenSpan.Width(1))
        tillNextWhitespaceRaw.unexpectedToken("\ud83daaa", 2, true) shouldBe Token.Named("non-printable character (\\ud83d)", TokenSpan.Width(1))
    }
    it should "refuse to print non-printable supplementary characters" in {
        tillNextWhitespaceTrimmed.unexpectedToken(Character.toChars(0x0f0000), 1, true) shouldBe Token.Named("non-printable codepoint (\\udb80\\udc00, or 0x0f0000)", TokenSpan.Width(1))
        tillNextWhitespaceTrimmed.unexpectedToken(Character.toChars(0x10ffff), 1, true) shouldBe Token.Named("non-printable codepoint (\\udbff\\udfff, or 0x10ffff)", TokenSpan.Width(1))
        tillNextWhitespaceRaw.unexpectedToken(Character.toChars(0x0f0000), 1, true) shouldBe Token.Named("non-printable codepoint (\\udb80\\udc00, or 0x0f0000)", TokenSpan.Width(1))
        tillNextWhitespaceRaw.unexpectedToken(Character.toChars(0x10ffff), 1, true) shouldBe Token.Named("non-printable codepoint (\\udbff\\udfff, or 0x10ffff)", TokenSpan.Width(1))
    }
    // TODO: lexToken
}

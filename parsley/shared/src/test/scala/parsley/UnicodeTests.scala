/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley

import Predef.{ArrowAssoc => _, *}

import parsley.unicode.*

class UnicodeTests extends ParsleyTest {
    // TODO: property-based testing for this!
    "item" should "accept any character" in {
        for (i <- (0x00000 to 0x0000a) ++ (0x00040 to 0x000ef) ++ (0x0ff00 to 0x0ff0a) ++ (0x1ff00 to 0x1ffff)) {
            item.parse(Character.toChars(i).mkString) should not be a [Failure[?]]
        }
    }
    it should "fail if the input has run out, expecting any character" in {
        inside(item.parse("")) {
            case Failure(TestError((1, 1), VanillaError(unex, exs, rs, 1))) =>
                unex should contain (EndOfInput)
                exs should contain only (Named("any character"))
                rs shouldBe empty
        }
    }

    "space" should "consume ' ' or '\t'" in cases(space)(
        " " -> Some(' '),
        "\t" -> Some('\t'),
        "\u000b" -> None,
    )

    "spaces" should "consume lots of spaces" in cases(spaces *> char('a'))(
        (" \t" * 5 + 'a') -> Some('a')
    )
    it should "never fail" in cases(spaces *> char('a'))(
        "a" -> Some('a')
    )

    // FIXME: this needs to be improved
    "whitespace" should "consume any whitespace chars" in {
        (whitespaces *> char('a')).parse(" \t\n\r\f\u000b" * 2 + 'a') should not be a [Failure[?]]
    }

    "endOfLine" should "consume windows or unix line endings" in cases(endOfLine)(
        "\n" -> Some('\n'),
        "\r\n" -> Some('\n'),
    )
    it should "fail otherwise" in {
        endOfLine.parse("a") shouldBe a [Failure[?]]
        endOfLine.parse("\r") shouldBe a [Failure[?]]
        endOfLine.parse("\r ") shouldBe a [Failure[?]]
        endOfLine.parse("  ") shouldBe a [Failure[?]]
    }

    "letter" should "accept non-latin characters" in cases(letter)(
        "ß" -> Some('ß'),
        "ð" -> Some('ð'),
        "é" -> Some('é'),
        "Å" -> Some('Å'),
        "λ" -> Some('λ'),
        "Ω" -> Some('Ω'),
    )
    it should "not accept raw high-surrogates but parse supplemental letters" in cases(letter)(
        "\ud840" -> None,
        "\ud87e\udc1a" -> Some(0x2f81a),
    )

    "upper" should "only accept uppercase characters" in {
        for (c <- 'A' to 'Z') upper.parse(c.toString) shouldBe Success(c)
        upper.parse("Ω") shouldBe Success('Ω')
        upper.parse("Å") shouldBe Success('Å')
    }
    it should "fail otherwise" in {
        for (c <- 'a' to 'z') upper.parse(c.toString) shouldBe a [Failure[?]]
        upper.parse("ß") shouldBe a [Failure[?]]
        upper.parse("ð") shouldBe a [Failure[?]]
        upper.parse("é") shouldBe a [Failure[?]]
        upper.parse("λ") shouldBe a [Failure[?]]
    }

    "lower" should "only accept lowercase characters" in {
        for (c <- 'a' to 'z') lower.parse(c.toString) shouldBe Success(c)
        lower.parse("ß") shouldBe Success('ß')
        lower.parse("ð") shouldBe Success('ð')
        lower.parse("é") shouldBe Success('é')
        lower.parse("λ") shouldBe Success('λ')
    }
    it should "fail otherwise" in {
        for (c <- 'A' to 'Z') lower.parse(c.toString) shouldBe a [Failure[?]]
        lower.parse("Ω") shouldBe a [Failure[?]]
        lower.parse("Å") shouldBe a [Failure[?]]
    }

    "digit parsers" should "accept the appropriate characters" in {
        for (c <- ('0' to '9') ++ ('\u0660' to '\u0669') ++ ('\uff10' to '\uff19')) {
            digit.parse(c.toString) shouldBe Success(c)
            hexDigit.parse(c.toString) shouldBe Success(c)
            letterOrDigit.parse(c.toString) shouldBe Success(c)
            val d = c.asDigit
            if (d >= 0 && d < 2) { val _ = bit.parse(c.toString) shouldBe Success(c) }
            if (d >= 0 && d < 8) { val _ = octDigit.parse(c.toString) shouldBe Success(c) }
        }
        for (c <- ('a' to 'f') ++ ('\uff41' to '\uff46') ++ ('A' to 'F') ++ ('\uff21' to '\uff26')) hexDigit.parse(c.toString) shouldBe Success(c)
    }
    they should "fail otherwise" in {
        for (c <- ('a' to 'f') ++ ('A' to 'F')) {
            bit.parse(c.toString) shouldBe a [Failure[?]]
            digit.parse(c.toString) shouldBe a [Failure[?]]
            octDigit.parse(c.toString) shouldBe a [Failure[?]]
        }
        bit.parse("2") shouldBe a [Failure[?]]
        octDigit.parse("8") shouldBe a [Failure[?]]
        octDigit.parse("9") shouldBe a [Failure[?]]
    }

    "oneOf" should "match any of the characters provided" in {
        cases(unicode.oneOf('a', 'b', 'c'))  ("a" -> Some('a'), "b" -> Some('b'), "c" -> Some('c'), "d" -> None)
        cases(unicode.oneOf(97 to 99))       ("a" -> Some('a'), "b" -> Some('b'), "c" -> Some('c'), "d" -> None)
        cases(unicode.oneOf(97 to 100 by 2))("a" -> Some('a'), "b" -> None, "c" -> Some('c'), "d" -> None)
    }
    it should "always fail if provided no characters" in {
        cases(unicode.oneOf())         ("a" -> None, "\n" -> None, "0" -> None)
        cases(unicode.oneOf(0 until 0))("a" -> None, "\n" -> None, "0" -> None)
    }
    it should "work for single character ranges too" in {
        cases(unicode.oneOf('a'))       ("a" -> Some('a'), "\n" -> None, "b" -> None)
        cases(unicode.oneOf(97 to 97))("a" -> Some('a'), "\n" -> None, "b" -> None)
    }

    "noneOf" should "match none of the characters provided" in {
        cases(unicode.noneOf('a', 'b', 'c'))  ("a" -> None, "b" -> None, "c" -> None, "d" -> Some('d'))
        cases(unicode.noneOf(97 to 99))     ("a" -> None, "b" -> None, "c" -> None, "d" -> Some('d'))
        cases(unicode.noneOf(97 to 100 by 2))("a" -> None, "b" -> Some('b'), "c" -> None, "d" -> Some('d'))
    }
    it should "match anything if provided no characters" in {
        cases(unicode.noneOf())             ("a" -> Some('a'), "\n" -> Some('\n'), "0" -> Some('0'))
        cases(unicode.noneOf(0 until 0))("a" -> Some('a'), "\n" -> Some('\n'), "0" -> Some('0'))
    }
    it should "work for single character ranges too" in {
        cases(unicode.noneOf('a'))       ("a" -> None, "\n" -> Some('\n'), "b" -> Some('b'))
        cases(unicode.noneOf(97 to 97))("a" -> None, "\n" -> Some('\n'), "b" -> Some('b'))
    }

    "char" should "handle BMP characters" in {
        cases(char('a'))("a" -> Some('a'))
        cases(char('λ'))("λ" -> Some('λ'))
    }

    it should "handle multi-character codepoints" in {
        cases(char(0x1F642))("🙂" -> Some(0x1F642))
    }

    it should "handle multi-character codepoints atomically on fail" in {
        cases(char(0x1F642) <|> char(0x1F643))("🙃" -> Some(0x1F643))
    }
}

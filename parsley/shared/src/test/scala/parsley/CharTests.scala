/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley

import Predef.{ArrowAssoc => _, _}

import parsley.character._

class CharTests extends ParsleyTest {
    // TODO: property-based testing for this!
    "item" should "accept any character" in {
        for (i <- ('\u0000' to '\u000a') ++ ('\u0040' to '\u00ef') ++ ('\uff00' to '\uff0a')) item.parse(i.toString) should not be a [Failure[_]]
    }
    it should "fail if the input has run out, expecting any character" in {
        inside(item.parse("")) {
            case Failure(TestError((1, 1), VanillaError(unex, exs, rs))) =>
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
        (whitespaces *> char('a')).parse(" \t\n\r\f\u000b" * 2 + 'a') should not be a [Failure[_]]
    }

    "endOfLine" should "consume windows or unix line endings" in cases(endOfLine)(
        "\n" -> Some('\n'),
        "\r\n" -> Some('\n'),
    )
    it should "fail otherwise" in {
        endOfLine.parse("a") shouldBe a [Failure[_]]
        endOfLine.parse("\r") shouldBe a [Failure[_]]
        endOfLine.parse("\r ") shouldBe a [Failure[_]]
        endOfLine.parse("  ") shouldBe a [Failure[_]]
    }

    "letter" should "accept non-latin characters" in cases(letter)(
        "ß" -> Some('ß'),
        "ð" -> Some('ð'),
        "é" -> Some('é'),
        "Å" -> Some('Å'),
        "λ" -> Some('λ'),
        "Ω" -> Some('Ω'),
    )
    it should "not accept high surrogates" in cases(letter)(
        "\ud840" -> None,
        "\ud87e\udc1a" -> None,
    )

    "upper" should "only accept uppercase characters" in {
        for (c <- 'A' to 'Z') upper.parse(c.toString) shouldBe Success(c)
        upper.parse("Ω") shouldBe Success('Ω')
        upper.parse("Å") shouldBe Success('Å')
    }
    it should "fail otherwise" in {
        for (c <- 'a' to 'z') upper.parse(c.toString) shouldBe a [Failure[_]]
        upper.parse("ß") shouldBe a [Failure[_]]
        upper.parse("ð") shouldBe a [Failure[_]]
        upper.parse("é") shouldBe a [Failure[_]]
        upper.parse("λ") shouldBe a [Failure[_]]
    }

    "lower" should "only accept lowercase characters" in {
        for (c <- 'a' to 'z') lower.parse(c.toString) shouldBe Success(c)
        lower.parse("ß") shouldBe Success('ß')
        lower.parse("ð") shouldBe Success('ð')
        lower.parse("é") shouldBe Success('é')
        lower.parse("λ") shouldBe Success('λ')
    }
    it should "fail otherwise" in {
        for (c <- 'A' to 'Z') lower.parse(c.toString) shouldBe a [Failure[_]]
        lower.parse("Ω") shouldBe a [Failure[_]]
        lower.parse("Å") shouldBe a [Failure[_]]
    }

    "digit parsers" should "accept the appropriate characters" in {
        for (c <- ('0' to '9') ++ ('\u0660' to '\u0669') ++ ('\uff10' to '\uff19')) {
            digit.parse(c.toString) shouldBe Success(c)
            hexDigit.parse(c.toString) shouldBe Success(c)
            val d = c.asDigit
            if (d >= 0 && d < 2) { val _ = bit.parse(c.toString) shouldBe Success(c) }
            if (d >= 0 && d < 8) { val _ = octDigit.parse(c.toString) shouldBe Success(c) }
        }
        for (c <- ('a' to 'f') ++ ('\uff41' to '\uff46') ++ ('A' to 'F') ++ ('\uff21' to '\uff26')) hexDigit.parse(c.toString) shouldBe Success(c)
    }
    they should "fail otherwise" in {
        for (c <- ('a' to 'f') ++ ('A' to 'F')) {
            bit.parse(c.toString) shouldBe a [Failure[_]]
            digit.parse(c.toString) shouldBe a [Failure[_]]
            octDigit.parse(c.toString) shouldBe a [Failure[_]]
        }
        bit.parse("2") shouldBe a [Failure[_]]
        octDigit.parse("8") shouldBe a [Failure[_]]
        octDigit.parse("9") shouldBe a [Failure[_]]
    }

    "oneOf" should "match any of the characters provided" in {
        cases(character.oneOf('a', 'b', 'c'))  ("a" -> Some('a'), "b" -> Some('b'), "c" -> Some('c'), "d" -> None)
        cases(character.oneOf('a' to 'c'))     ("a" -> Some('a'), "b" -> Some('b'), "c" -> Some('c'), "d" -> None)
        cases(character.oneOf('a' to 'd' by 2))("a" -> Some('a'), "b" -> None, "c" -> Some('c'), "d" -> None)
    }
    it should "always fail if provided no characters" in {
        cases(character.oneOf())             ("a" -> None, "\n" -> None, "0" -> None)
        cases(character.oneOf('0' until '0'))("a" -> None, "\n" -> None, "0" -> None)
    }
    it should "work for single character ranges too" in {
        cases(character.oneOf('a'))       ("a" -> Some('a'), "\n" -> None, "b" -> None)
        cases(character.oneOf('a' to 'a'))("a" -> Some('a'), "\n" -> None, "b" -> None)
    }

    "noneOf" should "match none of the characters provided" in {
        cases(character.noneOf('a', 'b', 'c'))  ("a" -> None, "b" -> None, "c" -> None, "d" -> Some('d'))
        cases(character.noneOf('a' to 'c'))     ("a" -> None, "b" -> None, "c" -> None, "d" -> Some('d'))
        cases(character.noneOf('a' to 'd' by 2))("a" -> None, "b" -> Some('b'), "c" -> None, "d" -> Some('d'))
    }
    it should "match anything if provided no characters" in {
        cases(character.noneOf())             ("a" -> Some('a'), "\n" -> Some('\n'), "0" -> Some('0'))
        cases(character.noneOf('0' until '0'))("a" -> Some('a'), "\n" -> Some('\n'), "0" -> Some('0'))
    }
    it should "work for single character ranges too" in {
        cases(character.noneOf('a'))       ("a" -> None, "\n" -> Some('\n'), "b" -> Some('b'))
        cases(character.noneOf('a' to 'a'))("a" -> None, "\n" -> Some('\n'), "b" -> Some('b'))
    }
}

/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley

import Predef.{ArrowAssoc => _, _}

import parsley.character._
import parsley.implicits.character.charLift

class CharTests extends ParsleyTest {
    "item" should "accept any character" in {
        for (i <- 0 to 65535) item.parse(i.toChar.toString) should not be a [Failure[_]]
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
    )
    it should "expect space/tab otherwise" in {
        for (i <- 0 to 65535; if i != ' ' && i != '\t') space.parse(i.toChar.toString) shouldBe a [Failure[_]]
    }

    "spaces" should "consume lots of spaces" in cases(spaces *> 'a')(
        (" \t" * 100 + 'a') -> Some('a')
    )
    it should "never fail" in cases(spaces *> 'a')(
        "a" -> Some('a')
    )

    "whitespace" should "consume any whitespace chars" in {
        (whitespaces *> 'a').parse(" \t\n\r\f\u000b" * 100 + 'a') should not be a [Failure[_]]
    }
    it should "fail otherwise" in {
        val cs = " \t\n\r\f\u000b".toSet
        for (i <- 0 to 65535; if !cs.contains(i.toChar)) whitespace.parse(i.toChar.toString) shouldBe a [Failure[_]]
    }

    "endOfLine" should "consume windows or unix line endings" in cases(endOfLine)(
        "\n" -> Some('\n'),
        "\r\n" -> Some('\n'),
    )
    it should "fail otherwise" in {
        for (i <- 0 to 65535; if i != 10) endOfLine.parse(i.toChar.toString) shouldBe a [Failure[_]]
    }

    "upper" should "only accept uppercase characters" in {
        for (c <- 'A' to 'Z') upper.parse(c.toString) shouldBe Success(c)
    }
    it should "fail otherwise" in {
        for (c <- 'a' to 'z') upper.parse(c.toString) shouldBe a [Failure[_]]
    }

    "lower" should "only accept lowercase characters" in {
        for (c <- 'a' to 'z') lower.parse(c.toString) shouldBe Success(c)
    }
    it should "fail otherwise" in {
        for (c <- 'A' to 'Z') lower.parse(c.toString) shouldBe a [Failure[_]]
    }

    "digit parsers" should "accept the appropriate characters" in {
        for (c <- '0' to '9') {
            digit.parse(c.toString) shouldBe Success(c)
            hexDigit.parse(c.toString) shouldBe Success(c)
            if (c < '8') {
                val _ = octDigit.parse(c.toString) shouldBe Success(c)
            }
        }
        for (c <- 'a' to 'f') hexDigit.parse(c.toString) shouldBe Success(c)
        for (c <- 'A' to 'F') hexDigit.parse(c.toString) shouldBe Success(c)
    }
    they should "fail otherwise" in {
        for (c <- 'a' to 'f') {
            digit.parse(c.toString) shouldBe a [Failure[_]]
            octDigit.parse(c.toString) shouldBe a [Failure[_]]
        }
        for (c <- 'A' to 'F') {
            digit.parse(c.toString) shouldBe a [Failure[_]]
            octDigit.parse(c.toString) shouldBe a [Failure[_]]
        }
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

    "charUtf16" should "handle BMP characters" in {
        cases(codePoint('a'))("a" -> Some('a'))
        cases(codePoint('λ'))("λ" -> Some('λ'))
    }

    it should "handle multi-character codepoints" in {
        cases(codePoint(0x1F642))("🙂" -> Some(0x1F642))
    }

    it should "handle multi-character codepoints atomically on fail" in {
        cases(codePoint(0x1F642) <|> codePoint(0x1F643))("🙃" -> Some(0x1F643))
    }
}

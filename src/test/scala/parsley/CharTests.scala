package parsley

import parsley.character._
import parsley.Parsley._
import parsley.implicits.character.{charLift, stringLift}

import scala.language.implicitConversions

class CharTests extends ParsleyTest {
    "string" should "consume succeed if it is found at head" in {
        "abc".parse("abc") should not be a [Failure[_]]
    }
    it should "not consume input if it fails on first character" in {
        ("abc" <|> 'b').parse("b") should not be a [Failure[_]]
    }
    it should "consume input if it fails mid-string" in {
        ("abc" <|> "ab").parse("ab") shouldBe a [Failure[_]]
    }
    it should "not consume input if it fails mid-string when combined with attempt" in {
        (attempt("abc") <|> "ab").parse("ab") should not be a [Failure[_]]
    }

    "anyChar" should "accept any character" in {
        for (i <- 0 to 65535) anyChar.parse(i.toChar.toString) should not be a [Failure[_]]
    }
    it should "fail if the input has run out, expecting any character" in {
        anyChar.parse("") should be (Failure("(line 1, column 1):\n  unexpected end of input\n  expected any character\n  >\n  >^"))
    }

    "space" should "consume ' ' or '\t'" in {
        space.parse(" ") should not be a [Failure[_]]
        space.parse("\t") should not be a [Failure[_]]
    }
    it should "expect space/tab otherwise" in {
        for (i <- 0 to 65535; if i != ' ' && i != '\t') space.parse(i.toChar.toString) shouldBe a [Failure[_]]
    }

    "spaces" should "consume lots of spaces" in {
        (spaces *> 'a').parse(" \t" * 100 + 'a') should not be a [Failure[_]]
    }
    it should "never fail" in {
        (spaces *> 'a').parse("a") should not be a [Failure[_]]
    }

    "whitespace" should "consume any whitespace chars" in {
        (whitespaces *> 'a').parse(" \t\n\r\f\u000b" * 100 + 'a') should not be a [Failure[_]]
    }
    it should "fail otherwise" in {
        val cs = " \t\n\r\f\u000b".toSet
        for (i <- 0 to 65535; if !cs.contains(i.toChar)) whitespace.parse(i.toChar.toString) shouldBe a [Failure[_]]
    }

    "endOfLine" should "consume windows or unix line endings" in {
        endOfLine.parse("\n") should not be a [Failure[_]]
        endOfLine.parse("\r\n") should not be a [Failure[_]]
    }
    it should "fail otherwise" in {
        for (i <- 0 to 65535; if i != 10) endOfLine.parse(i.toChar.toString) shouldBe a [Failure[_]]
    }

    "upper" should "only accept uppercase characters" in {
        for (c <- 'A' to 'Z') upper.parse(c.toString) should not be a [Failure[_]]
    }
    it should "fail otherwise" in {
        for (c <- 'a' to 'z') upper.parse(c.toString) shouldBe a [Failure[_]]
    }

    "lower" should "only accept lowercase characters" in {
        for (c <- 'a' to 'z') lower.parse(c.toString) should not be a [Failure[_]]
    }
    it should "fail otherwise" in {
        for (c <- 'A' to 'Z') lower.parse(c.toString) shouldBe a [Failure[_]]
    }

    "digit parsers" should "accept the appropriate characters" in {
        for (c <- '0' to '9') {
            digit.parse(c.toString) should not be a [Failure[_]]
            hexDigit.parse(c.toString) should not be a [Failure[_]]
            if (c < '8') octDigit.parse(c.toString) should not be a [Failure[_]]
        }
        for (c <- 'a' to 'f') hexDigit.parse(c.toString) should not be a [Failure[_]]
        for (c <- 'A' to 'F') hexDigit.parse(c.toString) should not be a [Failure[_]]
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
        val p = character.oneOf('a', 'b', 'c')
        p.parse("a") should not be a [Failure[_]]
        p.parse("b") should not be a [Failure[_]]
        p.parse("c") should not be a [Failure[_]]
        p.parse("d") shouldBe a [Failure[_]]
    }

    "noneOf" should "match none of the characters provided" in {
        val p = character.noneOf('a', 'b', 'c')
        p.parse("a") shouldBe a [Failure[_]]
        p.parse("b") shouldBe a [Failure[_]]
        p.parse("c") shouldBe a [Failure[_]]
        p.parse("d") should not be a [Failure[_]]
    }
}

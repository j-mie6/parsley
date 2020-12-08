package parsley

import parsley.Char._
import parsley.Parsley._
import parsley.Implicits.{charLift, stringLift}

import scala.language.implicitConversions

class CharTests extends ParsleyTest {
    "string" should "consume succeed if it is found at head" in {
        "abc".runParser("abc") should not be a [Failure]
    }
    it should "not consume input if it fails on first character" in {
        ("abc" <|> 'b').runParser("b") should not be a [Failure]
    }
    it should "consume input if it fails mid-string" in {
        ("abc" <|> "ab").runParser("ab") shouldBe a [Failure]
    }
    it should "not consume input if it fails mid-string when combined with attempt" in {
        ("abc" <\> "ab").runParser("ab") should not be a [Failure]
    }

    "anyChar" should "accept any character" in {
        for (i <- 0 to 65535) anyChar.runParser(i.toChar.toString) should not be a [Failure]
    }
    it should "fail if the input has run out, expecting any character" in {
        anyChar.runParser("") should be (Failure("(line 1, column 1):\n  unexpected end of input\n  expected any character"))
    }

    "space" should "consume ' ' or '\t'" in {
        space.runParser(" ") should not be a [Failure]
        space.runParser("\t") should not be a [Failure]
    }
    it should "expect space/tab otherwise" in {
        for (i <- 0 to 65535; if i != ' ' && i != '\t') space.runParser(i.toChar.toString) should be {
            Failure("(line 1, column 1):\n  unexpected \"" + i.toChar + "\"\n  expected space/tab")
        }
    }

    "spaces" should "consume lots of spaces" in {
        (spaces *> 'a').runParser(" \t" * 100 + 'a') should not be a [Failure]
    }
    it should "never fail" in {
        (spaces *> 'a').runParser("a") should not be a [Failure]
    }

    "whitespace" should "consume any whitespace chars" in {
        (whitespaces *> 'a').runParser(" \t\n\r\f\u000b" * 100 + 'a') should not be a [Failure]
    }
    it should "fail otherwise" in {
        val cs = " \t\n\r\f\u000b".toSet
        for (i <- 0 to 65535; if !cs.contains(i.toChar)) whitespace.runParser(i.toChar.toString) shouldBe a [Failure]
    }

    "endOfLine" should "consume windows or unix line endings" in {
        runParser(endOfLine, "\n") should not be a [Failure]
        runParser(endOfLine, "\r\n") should not be a [Failure]
    }
    it should "fail otherwise" in {
        for (i <- 0 to 65535; if i != 10) endOfLine.runParser(i.toChar.toString) shouldBe a [Failure]
    }

    "upper" should "only accept uppercase characters" in {
        for (c <- 'A' to 'Z') upper.runParser(c.toString) should not be a [Failure]
    }
    it should "fail otherwise" in {
        for (c <- 'a' to 'z') upper.runParser(c.toString) shouldBe a [Failure]
    }

    "lower" should "only accept lowercase characters" in {
        for (c <- 'a' to 'z') lower.runParser(c.toString) should not be a [Failure]
    }
    it should "fail otherwise" in {
        for (c <- 'A' to 'Z') lower.runParser(c.toString) shouldBe a [Failure]
    }

    "digit parsers" should "accept the appropriate characters" in {
        for (c <- '0' to '9') {
            digit.runParser(c.toString) should not be a [Failure]
            hexDigit.runParser(c.toString) should not be a [Failure]
            if (c < '8') octDigit.runParser(c.toString) should not be a [Failure]
        }
        for (c <- 'a' to 'f') hexDigit.runParser(c.toString) should not be a [Failure]
        for (c <- 'A' to 'F') hexDigit.runParser(c.toString) should not be a [Failure]
    }
    they should "fail otherwise" in {
        for (c <- 'a' to 'f') {
            digit.runParser(c.toString) shouldBe a [Failure]
            octDigit.runParser(c.toString) shouldBe a [Failure]
        }
        for (c <- 'A' to 'F') {
            digit.runParser(c.toString) shouldBe a [Failure]
            octDigit.runParser(c.toString) shouldBe a [Failure]
        }
        octDigit.runParser("8") shouldBe a [Failure]
        octDigit.runParser("9") shouldBe a [Failure]
    }
}

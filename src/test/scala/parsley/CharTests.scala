package parsley

import parsley.character._
import parsley.Parsley._
import parsley.implicits.character.charLift

import scala.language.implicitConversions

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
        val q = character.oneOf('a' to 'c')
        val r = character.oneOf('a' to 'd' by 2)
        p.parse("a") should not be a [Failure[_]]
        p.parse("b") should not be a [Failure[_]]
        p.parse("c") should not be a [Failure[_]]
        p.parse("d") shouldBe a [Failure[_]]
        q.parse("a") should not be a [Failure[_]]
        q.parse("b") should not be a [Failure[_]]
        q.parse("c") should not be a [Failure[_]]
        q.parse("d") shouldBe a [Failure[_]]
        r.parse("a") should not be a [Failure[_]]
        r.parse("b") shouldBe a [Failure[_]]
        r.parse("c") should not be a [Failure[_]]
        r.parse("d") shouldBe a [Failure[_]]
    }
    it should "always fail if provided no characters" in {
        val p = character.oneOf()
        val q = character.oneOf('0' until '0')
        p.parse("a") shouldBe a [Failure[_]]
        p.parse("\n") shouldBe a [Failure[_]]
        p.parse("0") shouldBe a [Failure[_]]
        q.parse("a") shouldBe a [Failure[_]]
        q.parse("\n") shouldBe a [Failure[_]]
        q.parse("0") shouldBe a [Failure[_]]
    }
    it should "work for single character ranges too" in {
        val p = character.oneOf('a')
        val q = character.oneOf('a' to 'a')
        p.parse("a") shouldBe a [Success[_]]
        p.parse("\n") shouldBe a [Failure[_]]
        p.parse("b") shouldBe a [Failure[_]]
        q.parse("a") shouldBe a [Success[_]]
        q.parse("\n") shouldBe a [Failure[_]]
        q.parse("b") shouldBe a [Failure[_]]
    }

    "noneOf" should "match none of the characters provided" in {
        val p = character.noneOf('a', 'b', 'c')
        val q = character.noneOf('a' to 'c')
        val r = character.noneOf('a' to 'd' by 2)
        p.parse("a") shouldBe a [Failure[_]]
        p.parse("b") shouldBe a [Failure[_]]
        p.parse("c") shouldBe a [Failure[_]]
        p.parse("d") should not be a [Failure[_]]
        q.parse("a") shouldBe a [Failure[_]]
        q.parse("b") shouldBe a [Failure[_]]
        q.parse("c") shouldBe a [Failure[_]]
        q.parse("d") should not be a [Failure[_]]
        r.parse("a") shouldBe a [Failure[_]]
        r.parse("b") should not be a [Failure[_]]
        r.parse("c") shouldBe a [Failure[_]]
        r.parse("d") should not be a [Failure[_]]
    }
    it should "match anything if provided no characters" in {
        val p = character.noneOf()
        val q = character.noneOf('0' until '0')
        p.parse("a") shouldBe a [Success[_]]
        p.parse("\n") shouldBe a [Success[_]]
        p.parse("0") shouldBe a [Success[_]]
        q.parse("a") shouldBe a [Success[_]]
        q.parse("\n") shouldBe a [Success[_]]
        q.parse("0") shouldBe a [Success[_]]
    }
    it should "work for single character ranges too" in {
        val p = character.noneOf('a')
        val q = character.noneOf('a' to 'a')
        p.parse("a") shouldBe a [Failure[_]]
        p.parse("\n") shouldBe a [Success[_]]
        p.parse("b") shouldBe a [Success[_]]
        q.parse("a") shouldBe a [Failure[_]]
        q.parse("\n") shouldBe a [Success[_]]
        q.parse("b") shouldBe a [Success[_]]
    }
}

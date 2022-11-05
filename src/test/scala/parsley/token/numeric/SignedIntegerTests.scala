package parsley.token.numeric

import parsley.{Parsley, ParsleyTest, Success, Failure}
import parsley.token.Lexeme
import parsley.token.descriptions.numeric._
import parsley.character.spaces

class SignedIntegerTests extends ParsleyTest {
    private def makeInteger(desc: NumericDesc) = new LexemeInteger(new SignedInteger(desc, new UnsignedInteger(desc)), Lexeme.empty)

    val plain = NumericDesc.plain.copy(integerNumbersCanBeBinary = true, literalBreakChar = BreakCharDesc.Supported('_', false))
    val optionalPlus = makeInteger(plain.copy(positiveSign = PlusSignPresence.Optional))
    val noPlus = makeInteger(plain.copy(positiveSign = PlusSignPresence.Illegal))
    val alwaysPlus = makeInteger(plain.copy(positiveSign = PlusSignPresence.Required))

    "signed decimal" should "parse valid decimal numbers of any size" in {
        optionalPlus.decimal.parseAll("0") shouldBe Success(0)
        optionalPlus.decimal.parseAll("+0123") shouldBe Success(123)
        optionalPlus.decimal.parseAll("-1230980485029") shouldBe Success(-1230980485029L)
        optionalPlus.decimal.parseAll("123098048502992634339") shouldBe Success(BigInt("123098048502992634339"))
    }

    it should "not allow for plus when configured" in {
        noPlus.decimal.parseAll("-0") shouldBe Success(0)
        noPlus.decimal.parseAll("+1") shouldBe a [Failure[_]]
        noPlus.decimal.parseAll("-1230980485029") shouldBe Success(-1230980485029L)
    }

    it should "require a plus when configured" in {
        alwaysPlus.decimal.parseAll("0") shouldBe a [Failure[_]]
        alwaysPlus.decimal.parseAll("+1") shouldBe Success(1)
        alwaysPlus.decimal.parseAll("-1230980485029") shouldBe Success(-1230980485029L)
    }

    it should "require at least one digit" in {
        optionalPlus.decimal.parseAll("") shouldBe a [Failure[_]]
        optionalPlus.decimal.parseAll("+") shouldBe a [Failure[_]]
        optionalPlus.decimal.parseAll("-") shouldBe a [Failure[_]]
        noPlus.decimal.parseAll("") shouldBe a [Failure[_]]
        noPlus.decimal.parseAll("-") shouldBe a [Failure[_]]
        alwaysPlus.decimal.parseAll("+") shouldBe a [Failure[_]]
        alwaysPlus.decimal.parseAll("-") shouldBe a [Failure[_]]
    }

    "signed hexadecimal" should "parse valid decimal numbers of any size" in {
        optionalPlus.hexadecimal.parseAll("0x0") shouldBe Success(0)
        optionalPlus.hexadecimal.parseAll("+0x0123") shouldBe Success(0x123)
        optionalPlus.hexadecimal.parseAll("-0x1230980485029") shouldBe Success(-0x1230980485029L)
        optionalPlus.hexadecimal.parseAll("0x123098048502992634339") shouldBe Success(BigInt("123098048502992634339", 16))
    }

    it should "not allow for plus when configured" in {
        noPlus.hexadecimal.parseAll("-0x0") shouldBe Success(0)
        noPlus.hexadecimal.parseAll("+0x1") shouldBe a [Failure[_]]
        noPlus.hexadecimal.parseAll("-0x1230980485029") shouldBe Success(-0x1230980485029L)
    }

    it should "require a plus when configured" in {
        alwaysPlus.hexadecimal.parseAll("0x0") shouldBe a [Failure[_]]
        alwaysPlus.hexadecimal.parseAll("+0x1") shouldBe Success(0x1)
        alwaysPlus.hexadecimal.parseAll("-0x1230980485029") shouldBe Success(-0x1230980485029L)
    }

    "unsigned octal" should "parse valid decimal numbers of any size" in {
        optionalPlus.octal.parseAll("0o0") shouldBe Success(0)
        optionalPlus.octal.parseAll("+0o0123") shouldBe Success(BigInt("123", 8))
        optionalPlus.octal.parseAll("-0o1230760465027") shouldBe Success(BigInt("-1230760465027", 8))
        optionalPlus.octal.parseAll("0o123036046502772634337") shouldBe Success(BigInt("123036046502772634337", 8))
    }

    it should "not allow for plus when configured" in {
        noPlus.octal.parseAll("-0o0") shouldBe Success(0)
        noPlus.octal.parseAll("+0o01") shouldBe a [Failure[_]]
        noPlus.octal.parseAll("-0o1230760465027") shouldBe Success(BigInt("-1230760465027", 8))
    }

    it should "require a plus when configured" in {
        alwaysPlus.octal.parseAll("0o0") shouldBe a [Failure[_]]
        alwaysPlus.octal.parseAll("+0o01") shouldBe Success(0x1)
        alwaysPlus.octal.parseAll("-0o1230760465027") shouldBe Success(BigInt("-1230760465027", 8))
    }

    "unsigned binary" should "parse valid decimal numbers of any size" in {
        optionalPlus.binary.parseAll("0b0") shouldBe Success(0)
        optionalPlus.binary.parseAll("+0b01010") shouldBe Success(BigInt("1010", 2))
        optionalPlus.binary.parseAll("-0b10111") shouldBe Success(BigInt("-10111", 2))
    }

    it should "not allow for leading zeros when configured" in {
        noPlus.binary.parseAll("0b0") shouldBe Success(0)
        noPlus.binary.parseAll("+0b01") shouldBe a [Failure[_]]
        noPlus.binary.parseAll("-0b10111") shouldBe Success(BigInt("-10111", 2))
    }

    it should "allow for literal break characters when configured" in {
        alwaysPlus.binary.parseAll("0b0") shouldBe a [Failure[_]]
        alwaysPlus.binary.parseAll("+0b01") shouldBe Success(1)
        alwaysPlus.binary.parseAll("-0b10111") shouldBe Success(BigInt("-10111", 2))
    }

    "number" should "support any of the different bases" in {
        noPlus.number.parseAll("123") shouldBe Success(123)
        noPlus.number.parseAll("0xff") shouldBe Success(0xff)
        alwaysPlus.number.parseAll("+0") shouldBe Success(0)
        alwaysPlus.number.parseAll("+0o17") shouldBe Success(15)
        optionalPlus.number.parseAll("-0b1010") shouldBe Success(-10)
    }

    "bounded numbers" should "also work across any different base" in {
        optionalPlus.binary8.parseAll("0b11010") shouldBe Success(26)
        optionalPlus.binary8.parseAll("0b0111_1111") shouldBe Success(127)
        optionalPlus.binary8.parseAll("0b1111_1111") shouldBe a [Failure[_]]
        optionalPlus.binary8.parseAll("-0b1000_0000") shouldBe Success(-128)
        optionalPlus.binary8[Long].parseAll("0b1_0000_0000") shouldBe a [Failure[_]]
        optionalPlus.binary8.parseAll("-0b1_0000_0000") shouldBe a [Failure[_]]
        optionalPlus.octal16.parseAll("0o404") shouldBe Success(260)
        optionalPlus.octal16.parseAll("0o303240") shouldBe a [Failure[_]]
        optionalPlus.hexadecimal16.parseAll("0x7fff") shouldBe Success(0x7fff)
        optionalPlus.hexadecimal16.parseAll("-0x8000") shouldBe Success(-0x8000)
        optionalPlus.hexadecimal16.parseAll("0x8000") shouldBe a [Failure[_]]
        optionalPlus.decimal32.parseAll("1239874") shouldBe Success(1239874)
        optionalPlus.number32.parseAll("0x7fff_ffff") shouldBe Success(0x7fffffff)
        optionalPlus.number32.parseAll("-0x8000_0000") shouldBe Success(-0x80000000)
        optionalPlus.number32.parseAll("0x8000_0000") shouldBe a [Failure[_]]
        optionalPlus.hexadecimal64.parseAll("0x1_0000_0000") shouldBe Success(0x100000000L)
        optionalPlus.hexadecimal64.parseAll("0x7fff_ffff_ffff_ffff") shouldBe Success(0x7fffffffffffffffL)
        optionalPlus.hexadecimal64.parseAll("-0x8000_0000_0000_0000") shouldBe Success(-0x8000000000000000L)
        optionalPlus.hexadecimal64.parseAll("0x8000_0000_0000_0000") shouldBe a [Failure[_]]
    }
}

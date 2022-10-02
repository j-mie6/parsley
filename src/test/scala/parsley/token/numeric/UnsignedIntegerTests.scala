package parsley.token.numeric

import parsley.{Parsley, ParsleyTest, Success, Failure}
import parsley.token.Lexeme
import parsley.token.descriptions.numeric._
import parsley.character.spaces

class UnsignedIntegerTests extends ParsleyTest {
    private def makeInteger(desc: NumericDesc) = new LexemeInteger(new UnsignedInteger(desc), Lexeme.empty)

    val plain = NumericDesc.plain
    val withLeadingZero = makeInteger(plain)
    val withoutLeadingZero = makeInteger(plain.copy(leadingZerosAllowed = false, integerNumbersCanBeBinary = true))
    val withLeadingZeroAndBreak = makeInteger(plain.copy(literalBreakChar = BreakCharDesc.Supported('_', true)))
    val withoutLeadingZeroAndBreak = makeInteger(plain.copy(leadingZerosAllowed = false, literalBreakChar = BreakCharDesc.Supported('_', true)))
    val withLeadingZeroAndBreakNotAfterPrefix = makeInteger(plain.copy(literalBreakChar = BreakCharDesc.Supported('_', false)))
    val withoutLeadingZeroAndBreakNotAfterPrefix = makeInteger(plain.copy(leadingZerosAllowed = false, literalBreakChar = BreakCharDesc.Supported('_', false)))

    "unsigned decimal" should "parse valid decimal numbers of any size" in {
        withLeadingZero.decimal.parseAll("0") shouldBe Success(0)
        withLeadingZero.decimal.parseAll("0123") shouldBe Success(123)
        withLeadingZero.decimal.parseAll("1230980485029") shouldBe Success(1230980485029L)
        withLeadingZero.decimal.parseAll("123098048502992634339") shouldBe Success(BigInt("123098048502992634339"))
    }

    it should "not allow for leading zeros when configured" in {
        withoutLeadingZero.decimal.parseAll("0") shouldBe Success(0)
        withoutLeadingZero.decimal.parseAll("01") shouldBe a [Failure[_]]
        withoutLeadingZero.decimal.parseAll("1230980485029") shouldBe Success(1230980485029L)
    }

    it should "allow for literal break characters when configured" in {
        withLeadingZeroAndBreak.decimal.parseAll("0") shouldBe Success(0)
        withLeadingZeroAndBreak.decimal.parseAll("0123") shouldBe Success(123)
        withLeadingZeroAndBreak.decimal.parseAll("1230980485029") shouldBe Success(1230980485029L)
        withLeadingZeroAndBreak.decimal.parseAll("0_123") shouldBe Success(123)
        withLeadingZeroAndBreak.decimal.parseAll("123_0_980_485029") shouldBe Success(1230980485029L)
        info("a trailing break should not be permitted")
        withLeadingZeroAndBreak.decimal.parseAll("0_") shouldBe a [Failure[_]]
        info("a leading break should not be permitted")
        withLeadingZeroAndBreak.decimal.parseAll("_9") shouldBe a [Failure[_]]
        info("it should not allow for double breaks, however")
        withLeadingZeroAndBreak.decimal.parseAll("123_0_980__485029") shouldBe a [Failure[_]]
    }

    it should "allow for literal breaks without leading zeros when configured" in {
        withoutLeadingZeroAndBreak.decimal.parseAll("0") shouldBe Success(0)
        withoutLeadingZeroAndBreak.decimal.parseAll("0123") shouldBe a [Failure[_]]
        withoutLeadingZeroAndBreak.decimal.parseAll("1230980485029") shouldBe Success(1230980485029L)
        withoutLeadingZeroAndBreak.decimal.parseAll("0_123") shouldBe a [Failure[_]]
        withoutLeadingZeroAndBreak.decimal.parseAll("1_123") shouldBe Success(1123)
        withoutLeadingZeroAndBreak.decimal.parseAll("123_0_980_485029") shouldBe Success(1230980485029L)
        info("a trailing break should not be permitted")
        withoutLeadingZeroAndBreak.decimal.parseAll("1_") shouldBe a [Failure[_]]
        info("a leading break should not be permitted")
        withoutLeadingZeroAndBreak.decimal.parseAll("_9") shouldBe a [Failure[_]]
        info("it should not allow for double breaks, however")
        withoutLeadingZeroAndBreak.decimal.parseAll("123_0_980__485029") shouldBe a [Failure[_]]
    }

    it should "require at least one digit" in {
        withoutLeadingZeroAndBreak.decimal.parseAll("") shouldBe a [Failure[_]]
    }

    "unsigned hexadecimal" should "parse valid decimal numbers of any size" in {
        withLeadingZero.hexadecimal.parseAll("0x0") shouldBe Success(0)
        withLeadingZero.hexadecimal.parseAll("0x0123") shouldBe Success(0x123)
        withLeadingZero.hexadecimal.parseAll("0x1230980485029") shouldBe Success(0x1230980485029L)
        withLeadingZero.hexadecimal.parseAll("0x1230f8048502992634339") shouldBe Success(BigInt("1230f8048502992634339", 16))
    }

    it should "not allow for leading zeros when configured" in {
        withoutLeadingZero.hexadecimal.parseAll("0x0") shouldBe Success(0)
        withoutLeadingZero.hexadecimal.parseAll("0x01") shouldBe a [Failure[_]]
        withoutLeadingZero.hexadecimal.parseAll("0x12f0980485029") shouldBe Success(0x12f0980485029L)
    }

    it should "allow for literal break characters when configured" in {
        withLeadingZeroAndBreak.hexadecimal.parseAll("0x0") shouldBe Success(0)
        withLeadingZeroAndBreak.hexadecimal.parseAll("0x0123") shouldBe Success(0x123)
        withLeadingZeroAndBreak.hexadecimal.parseAll("0x1230980485029") shouldBe Success(0x1230980485029L)
        withLeadingZeroAndBreak.hexadecimal.parseAll("0x0_123") shouldBe Success(0x123)
        withLeadingZeroAndBreak.hexadecimal.parseAll("0x123_0_980_485029") shouldBe Success(0x1230980485029L)
        info("a trailing break should not be permitted")
        withLeadingZeroAndBreak.hexadecimal.parseAll("0x0_") shouldBe a [Failure[_]]
        info("a leading break should not be permitted")
        withLeadingZeroAndBreak.hexadecimal.parseAll("0_x9") shouldBe a [Failure[_]]
        info("not after the prefix when configured")
        withLeadingZeroAndBreak.hexadecimal.parseAll("0x_9") shouldBe Success(9)
        withLeadingZeroAndBreakNotAfterPrefix.hexadecimal.parseAll("0x_9") shouldBe a [Failure[_]]
        info("it should not allow for double breaks, however")
        withLeadingZeroAndBreak.hexadecimal.parseAll("0x123_0_980__485029") shouldBe a [Failure[_]]
    }

    it should "allow for literal breaks without leading zeros when configured" in {
        withoutLeadingZeroAndBreak.hexadecimal.parseAll("0x0") shouldBe Success(0)
        withoutLeadingZeroAndBreak.hexadecimal.parseAll("0x0123") shouldBe a [Failure[_]]
        withoutLeadingZeroAndBreak.hexadecimal.parseAll("0x1230980485029") shouldBe Success(0x1230980485029L)
        withoutLeadingZeroAndBreak.hexadecimal.parseAll("0x0_123") shouldBe a [Failure[_]]
        withoutLeadingZeroAndBreak.hexadecimal.parseAll("0x1_123") shouldBe Success(0x1123)
        withoutLeadingZeroAndBreak.hexadecimal.parseAll("0x123_0_980_485029") shouldBe Success(0x1230980485029L)
        info("a trailing break should not be permitted")
        withoutLeadingZeroAndBreak.hexadecimal.parseAll("0x1_") shouldBe a [Failure[_]]
        info("a leading break should not be permitted")
        withoutLeadingZeroAndBreak.hexadecimal.parseAll("0_x9") shouldBe a [Failure[_]]
        info("not after the prefix when configured")
        withoutLeadingZeroAndBreak.hexadecimal.parseAll("0x_9") shouldBe Success(9)
        withoutLeadingZeroAndBreakNotAfterPrefix.hexadecimal.parseAll("0x_9") shouldBe a [Failure[_]]
        info("it should not allow for double breaks, however")
        withoutLeadingZeroAndBreak.hexadecimal.parseAll("0x123_0_980__485029") shouldBe a [Failure[_]]
    }

    it should "require at least one digit" in {
        withoutLeadingZeroAndBreak.hexadecimal.parseAll("0x") shouldBe a [Failure[_]]
    }

    "unsigned octal" should "parse valid decimal numbers of any size" in {
        withLeadingZero.octal.parseAll("0o0") shouldBe Success(0)
        withLeadingZero.octal.parseAll("0o0123") shouldBe Success(BigInt("123", 8))
        withLeadingZero.octal.parseAll("0o1230760465027") shouldBe Success(BigInt("1230760465027", 8))
        withLeadingZero.octal.parseAll("0o123036046502772634337") shouldBe Success(BigInt("123036046502772634337", 8))
    }

    it should "not allow for leading zeros when configured" in {
        withoutLeadingZero.octal.parseAll("0o0") shouldBe Success(0)
        withoutLeadingZero.octal.parseAll("0o01") shouldBe a [Failure[_]]
        withoutLeadingZero.octal.parseAll("0o1230760465027") shouldBe Success(BigInt("1230760465027", 8))
    }

    it should "allow for literal break characters when configured" in {
        withLeadingZeroAndBreak.octal.parseAll("0o0") shouldBe Success(0)
        withLeadingZeroAndBreak.octal.parseAll("0o0123") shouldBe Success(BigInt("123", 8))
        withLeadingZeroAndBreak.octal.parseAll("0o1230760465027") shouldBe Success(BigInt("1230760465027", 8))
        withLeadingZeroAndBreak.octal.parseAll("0o0_123") shouldBe Success(BigInt("123", 8))
        withLeadingZeroAndBreak.octal.parseAll("0o123_0_760_465027") shouldBe Success(BigInt("1230760465027", 8))
        info("a trailing break should not be permitted")
        withLeadingZeroAndBreak.octal.parseAll("0o0_") shouldBe a [Failure[_]]
        info("a leading break should not be permitted")
        withLeadingZeroAndBreak.octal.parseAll("0_o7") shouldBe a [Failure[_]]
        info("not after the prefix when configured")
        withLeadingZeroAndBreak.octal.parseAll("0o_7") shouldBe Success(7)
        withLeadingZeroAndBreakNotAfterPrefix.octal.parseAll("0o_7") shouldBe a [Failure[_]]
        info("it should not allow for double breaks, however")
        withLeadingZeroAndBreak.octal.parseAll("0o123_0_760__465027") shouldBe a [Failure[_]]
    }

    it should "allow for literal breaks without leading zeros when configured" in {
        withoutLeadingZeroAndBreak.octal.parseAll("0o0") shouldBe Success(0)
        withoutLeadingZeroAndBreak.octal.parseAll("0o0123") shouldBe a [Failure[_]]
        withoutLeadingZeroAndBreak.octal.parseAll("0o1230760465027") shouldBe Success(BigInt("1230760465027", 8))
        withoutLeadingZeroAndBreak.octal.parseAll("0o0_123") shouldBe a [Failure[_]]
        withoutLeadingZeroAndBreak.octal.parseAll("0o1_123") shouldBe Success(BigInt("1123", 8))
        withoutLeadingZeroAndBreak.octal.parseAll("0o123_0_760_465027") shouldBe Success(BigInt("1230760465027", 8))
        info("a trailing break should not be permitted")
        withoutLeadingZeroAndBreak.octal.parseAll("0o1_") shouldBe a [Failure[_]]
        info("a leading break should not be permitted")
        withoutLeadingZeroAndBreak.octal.parseAll("0_o7") shouldBe a [Failure[_]]
        info("not after the prefix when configured")
        withoutLeadingZeroAndBreak.octal.parseAll("0o_7") shouldBe Success(7)
        withoutLeadingZeroAndBreakNotAfterPrefix.octal.parseAll("0o_7") shouldBe a [Failure[_]]
        info("it should not allow for double breaks, however")
        withoutLeadingZeroAndBreak.octal.parseAll("0o123_0_760__465027") shouldBe a [Failure[_]]
    }

    it should "require at least one digit" in {
        withoutLeadingZeroAndBreak.octal.parseAll("0o") shouldBe a [Failure[_]]
    }

    "unsigned binary" should "parse valid decimal numbers of any size" in {
        withLeadingZero.binary.parseAll("0b0") shouldBe Success(0)
        withLeadingZero.binary.parseAll("0b01010") shouldBe Success(BigInt("1010", 2))
        withLeadingZero.binary.parseAll("0b10111") shouldBe Success(BigInt("10111", 2))
    }

    it should "not allow for leading zeros when configured" in {
        withoutLeadingZero.binary.parseAll("0b0") shouldBe Success(0)
        withoutLeadingZero.binary.parseAll("0b01") shouldBe a [Failure[_]]
        withoutLeadingZero.binary.parseAll("0b10111") shouldBe Success(BigInt("10111", 2))
    }

    it should "allow for literal break characters when configured" in {
        withLeadingZeroAndBreak.binary.parseAll("0b0") shouldBe Success(0)
        withLeadingZeroAndBreak.binary.parseAll("0b01010") shouldBe Success(BigInt("1010", 2))
        withLeadingZeroAndBreak.binary.parseAll("0b10111") shouldBe Success(BigInt("10111", 2))
        withLeadingZeroAndBreak.binary.parseAll("0b0_101") shouldBe Success(BigInt("101", 2))
        withLeadingZeroAndBreak.binary.parseAll("0b1010_0101_0001_1111") shouldBe Success(0xa51f)
        info("a trailing break should not be permitted")
        withLeadingZeroAndBreak.binary.parseAll("0b0_") shouldBe a [Failure[_]]
        info("a leading break should not be permitted")
        withLeadingZeroAndBreak.binary.parseAll("0_b1") shouldBe a [Failure[_]]
        info("not after the prefix when configured")
        withLeadingZeroAndBreak.binary.parseAll("0b_1") shouldBe Success(1)
        withLeadingZeroAndBreakNotAfterPrefix.binary.parseAll("0b_1") shouldBe a [Failure[_]]
        info("it should not allow for double breaks, however")
        withLeadingZeroAndBreak.binary.parseAll("0b1010_0101__0001_1111") shouldBe a [Failure[_]]
    }

    it should "allow for literal breaks without leading zeros when configured" in {
        withoutLeadingZeroAndBreak.binary.parseAll("0b0") shouldBe Success(0)
        withoutLeadingZeroAndBreak.binary.parseAll("0b01010") shouldBe a [Failure[_]]
        withoutLeadingZeroAndBreak.binary.parseAll("0b10111") shouldBe Success(BigInt("10111", 2))
        withoutLeadingZeroAndBreak.binary.parseAll("0b0_101") shouldBe a [Failure[_]]
        withoutLeadingZeroAndBreak.binary.parseAll("0b1_101") shouldBe Success(BigInt("1101", 2))
        withoutLeadingZeroAndBreak.binary.parseAll("0b1010_0101_0001_1111") shouldBe Success(0xa51f)
        info("a trailing break should not be permitted")
        withoutLeadingZeroAndBreak.binary.parseAll("0b1_") shouldBe a [Failure[_]]
        info("a leading break should not be permitted")
        withoutLeadingZeroAndBreak.binary.parseAll("0_b1") shouldBe a [Failure[_]]
        info("not after the prefix when configured")
        withoutLeadingZeroAndBreak.binary.parseAll("0b_1") shouldBe Success(1)
        withoutLeadingZeroAndBreakNotAfterPrefix.binary.parseAll("0b_1") shouldBe a [Failure[_]]
        info("it should not allow for double breaks, however")
        withoutLeadingZeroAndBreak.binary.parseAll("0b1010_0101__0001_1111") shouldBe a [Failure[_]]
    }

    it should "require at least one digit" in {
        withoutLeadingZeroAndBreak.binary.parseAll("0b") shouldBe a [Failure[_]]
    }

    "prefixless numbers" should "be supported with a leading 0" in {
        val lex = makeInteger(plain.copy(leadingZerosAllowed = false, octalLeads = Set.empty))
        lex.octal.parseAll("012") shouldBe Success(BigInt("12", 8))
        lex.number.parseAll("130") shouldBe Success(130)
        lex.number.parseAll("012") shouldBe Success(BigInt("12", 8))
    }

    "number" should "support any of the different bases" in {
        withoutLeadingZero.number.parseAll("123") shouldBe Success(123)
        withoutLeadingZero.number.parseAll("0xff") shouldBe Success(0xff)
        withoutLeadingZero.number.parseAll("0") shouldBe Success(0)
        withoutLeadingZero.number.parseAll("0o17") shouldBe Success(15)
        withoutLeadingZero.number.parseAll("0b1010") shouldBe Success(10)
    }

    "bounded numbers" should "also work across any different base" in {
        withoutLeadingZero.binary8.parseAll("0b11010") shouldBe Success(26)
        withoutLeadingZeroAndBreak.binary8[Long].parseAll("0b1_0000_0000") shouldBe a [Failure[_]]
        withoutLeadingZero.octal16.parseAll("0o404") shouldBe Success(260)
        withoutLeadingZero.octal16.parseAll("0o303240") shouldBe a [Failure[_]]
        withoutLeadingZero.decimal32.parseAll("1239874") shouldBe Success(1239874)
        withoutLeadingZero.number32.parseAll("0xffffffff") shouldBe Success(0xffffffff)
        withoutLeadingZeroAndBreak.number32.parseAll("0x1_0000_0000") shouldBe a [Failure[_]]
        withoutLeadingZeroAndBreak.hexadecimal64.parseAll("0x1_0000_0000") shouldBe Success(0x100000000L)
        withoutLeadingZeroAndBreak.hexadecimal64.parseAll("0xffff_ffff_ffff_ffff") shouldBe Success(0xffffffffffffffffL)
        withoutLeadingZeroAndBreak.hexadecimal64.parseAll("0x1_0000_0000_0000_0000") shouldBe a [Failure[_]]
    }
}

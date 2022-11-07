package parsley.token.numeric

import parsley.{Parsley, ParsleyTest, Success, Failure}
import parsley.token.Lexeme
import parsley.token.descriptions.numeric._, ExponentDesc.NoExponents
import parsley.character.spaces

class RealTests extends ParsleyTest {
    private def makeReal(desc: NumericDesc) = new LexemeReal(new SignedReal(desc, new UnsignedReal(desc, new UnsignedInteger(desc))), Lexeme.empty)

    val plain = NumericDesc.plain.copy(decimalExponentDesc = NoExponents, hexadecimalExponentDesc = NoExponents,
                                       octalExponentDesc = NoExponents, binaryExponentDesc = NoExponents)
    val withLeadingDotDesc = plain.copy(leadingDotAllowed = true)
    val withTrailingDotDesc = plain.copy(trailingDotAllowed = true)
    val withExtremeDotDesc = plain.copy(leadingDotAllowed = true, trailingDotAllowed = true)
    val withoutExtremeDotBreakDesc = plain.copy(literalBreakChar = BreakCharDesc.Supported('_', true))
    val withLeadingDotBreakDesc = plain.copy(leadingDotAllowed = true, literalBreakChar = BreakCharDesc.Supported('_', true))
    val withTrailingDotBreakDesc = plain.copy(trailingDotAllowed = true, literalBreakChar = BreakCharDesc.Supported('_', true))
    val withExtremeDotBreakDesc = plain.copy(leadingDotAllowed = true, trailingDotAllowed = true, literalBreakChar = BreakCharDesc.Supported('_', true))

    val withoutExtremeDot = makeReal(plain)
    val withLeadingDot = makeReal(withLeadingDotDesc)
    val withTrailingDot = makeReal(withTrailingDotDesc)
    val withExtremeDot = makeReal(withExtremeDotDesc)
    val withoutExtremeDotBreak = makeReal(withoutExtremeDotBreakDesc)
    val withLeadingDotBreak = makeReal(withLeadingDotBreakDesc)
    val withTrailingDotBreak = makeReal(withTrailingDotBreakDesc)
    val withExtremeDotBreak = makeReal(withExtremeDotBreakDesc)

    "decimal reals" should "parse unbounded real numbers" in {
        withoutExtremeDot.decimal.parseAll("0.0") shouldBe Success(BigDecimal("0"))
        withoutExtremeDot.decimal.parseAll("0.123") shouldBe Success(BigDecimal("0.123"))
        withoutExtremeDot.decimal.parseAll("123456789.987654321") shouldBe Success(BigDecimal("123456789.987654321"))
        withoutExtremeDot.decimal.parseAll("-3.142") shouldBe Success(BigDecimal("-3.142"))
        withoutExtremeDotBreak.decimal.parseAll("0.0") shouldBe Success(BigDecimal("0"))
        withoutExtremeDotBreak.decimal.parseAll("0.1_23") shouldBe Success(BigDecimal("0.123"))
        withoutExtremeDotBreak.decimal.parseAll("123456789.98_7654321") shouldBe Success(BigDecimal("123456789.987654321"))
        withoutExtremeDotBreak.decimal.parseAll("-3.1_42") shouldBe Success(BigDecimal("-3.142"))
    }
    it should "permit leading and trailing dots when applicable" in {
        withoutExtremeDot.decimal.parseAll("1.") shouldBe a [Failure[_]]
        withoutExtremeDot.decimal.parseAll(".234") shouldBe a [Failure[_]]
        withLeadingDot.decimal.parseAll("1.") shouldBe a [Failure[_]]
        withLeadingDot.decimal.parseAll(".234") shouldBe Success(BigDecimal("0.234"))
        withTrailingDot.decimal.parseAll("1.") shouldBe Success(BigDecimal("1.0"))
        withTrailingDot.decimal.parseAll(".234") shouldBe a [Failure[_]]
        withExtremeDot.decimal.parseAll("0.") shouldBe Success(BigDecimal("0"))
        withExtremeDot.decimal.parseAll(".0") shouldBe Success(BigDecimal("0"))
    }
    it should "not allow for both leading and trailing dots" in {
        withExtremeDot.decimal.parseAll(".") shouldBe a [Failure[_]]
    }
    it should "not allow for a break past a dot" in {
        withoutExtremeDotBreak.decimal.parseAll("0._1") shouldBe a [Failure[_]]
        withExtremeDotBreak.decimal.parseAll("0._1") shouldBe a [Failure[_]]
        withLeadingDotBreak.decimal.parseAll("0._1") shouldBe a [Failure[_]]
        withTrailingDotBreak.decimal.parseAll("0._1") shouldBe a [Failure[_]]
    }
    it should "allow for scientific notation when configured" in {
        val plainExp = plain
        val withExtremeDotExpDesc = plainExp.copy(leadingDotAllowed = true, trailingDotAllowed = true)
        val withoutExtremeDot = makeReal(plainExp)
        val withExtremeDot = makeReal(withExtremeDotExpDesc)

    }

    // hexadecimal (hexadecimalExponentDesc)
    "hexadecimal reals" should "parse unbounded real numbers" in pending

    // octal (octalExponentDesc)
    "octal reals" should "parse unbounded real numbers" in pending

    // binary (binaryExponentDesc)
    "binary reals" should "parse unbounded real numbers" in pending

    // number
    "generic reals" should "parse in any base" in pending

    // bounded things (only decimal)
    "bounded reals" should "not permit illegal numbers" in pending
}

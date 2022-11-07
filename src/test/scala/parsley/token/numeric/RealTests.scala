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

    private def decimalCases(real: Real)(tests: (String, Option[BigDecimal])*): Unit = cases(real.decimal)(tests: _*)
    private def hexadecimalCases(real: Real)(tests: (String, Option[BigDecimal])*): Unit = cases(real.hexadecimal)(tests: _*)
    private def octalCases(real: Real)(tests: (String, Option[BigDecimal])*): Unit = cases(real.octal)(tests: _*)
    private def binaryCases(real: Real)(tests: (String, Option[BigDecimal])*): Unit = cases(real.binary)(tests: _*)
    private def numberCases(real: Real)(tests: (String, Option[BigDecimal])*): Unit = cases(real.number)(tests: _*)

    private def decimalCases(desc: NumericDesc)(tests: (String, Option[BigDecimal])*): Unit = decimalCases(makeReal(desc))(tests: _*)
    private def hexadecimalCases(desc: NumericDesc)(tests: (String, Option[BigDecimal])*): Unit = hexadecimalCases(makeReal(desc))(tests: _*)
    private def octalCases(desc: NumericDesc)(tests: (String, Option[BigDecimal])*): Unit = octalCases(makeReal(desc))(tests: _*)
    private def binaryCases(desc: NumericDesc)(tests: (String, Option[BigDecimal])*): Unit = binaryCases(makeReal(desc))(tests: _*)
    private def numberCases(desc: NumericDesc)(tests: (String, Option[BigDecimal])*): Unit = numberCases(makeReal(desc))(tests: _*)

    "decimal reals" should "parse unbounded real numbers" in {
        decimalCases(withoutExtremeDot)(
            "0.0" -> Some(BigDecimal("0")),
            "0.123" -> Some(BigDecimal("0.123")),
            "123456789.987654321" -> Some(BigDecimal("123456789.987654321")),
            "-3.142" -> Some(BigDecimal("-3.142")),
        )
        decimalCases(withoutExtremeDotBreak)(
            "0.0" -> Some(BigDecimal("0")),
            "0.1_23" -> Some(BigDecimal("0.123")),
            "123456789.98_7654321" -> Some(BigDecimal("123456789.987654321")),
            "-3.1_42" -> Some(BigDecimal("-3.142")),
        )
    }
    it should "permit leading and trailing dots when applicable" in {
        decimalCases(withoutExtremeDot)("1." -> None, ".234" -> None)
        decimalCases(withLeadingDot)("1." -> None, ".234" -> Some(BigDecimal("0.234")))
        decimalCases(withTrailingDot)("1." -> Some(BigDecimal("1.0")), ".234" -> None)
        decimalCases(withExtremeDot)("0." -> Some(BigDecimal("0")), ".0" -> Some(BigDecimal("0")))
    }
    it should "not allow for both leading and trailing dots" in decimalCases(withExtremeDot)("." -> None)
    it should "not allow for a break past a dot" in {
        decimalCases(withoutExtremeDotBreak)("0._1" -> None)
        decimalCases(withExtremeDotBreak)("0._1" -> None)
        decimalCases(withLeadingDotBreak)("0._1" -> None)
        decimalCases(withTrailingDotBreak)("0._1" -> None)
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

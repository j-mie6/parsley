/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.numeric

import parsley.Parsley, Parsley.pure
import parsley.character, character.{isHexDigit, isOctDigit, satisfy}
import parsley.combinator.optional
import parsley.errors.combinator.ErrorMethods
import parsley.implicits.character.charLift
import parsley.token.descriptions.numeric.{BreakCharDesc, NumericDesc}
import parsley.token.errors.ErrorConfig

// These methods should not have labels on the leads, because that can interfere with their various uses
// ^ this is tricky, because we should mark the first digit with the "end of literal"... I guess we can mark the tail digit? bit sad
private [token] class Generic(err: ErrorConfig) {
    private def ofRadix(radix: Int, digit: Parsley[Char], label: Option[String]): Parsley[BigInt] = ofRadix(radix, digit, digit, label)
    private def ofRadix(radix: Int, startDigit: Parsley[Char], digit: Parsley[Char], label: Option[String]): Parsley[BigInt] = {
        val pf = pure((x: BigInt, d: Char) => x*radix + d.asDigit)
        val trailingDigit = ErrorConfig.label(label)(digit)
        parsley.expr.infix.secretLeft1(startDigit.map(d => BigInt(d.asDigit)), trailingDigit, pf)
    }

    private def ofRadix(radix: Int, digit: Parsley[Char], breakChar: Char, label: Option[String]): Parsley[BigInt] = {
        ofRadix(radix, digit, digit, breakChar, label)
    }
    private def ofRadix(radix: Int, startDigit: Parsley[Char], digit: Parsley[Char], breakChar: Char, label: Option[String]): Parsley[BigInt] = {
        val pf = pure((x: BigInt, d: Char) => x*radix + d.asDigit)
        val trailingDigit = ErrorConfig.label(label)(optional(ErrorConfig.explain(err.explainBreakChar)(breakChar)) *> digit)
        parsley.expr.infix.secretLeft1(startDigit.map(d => BigInt(d.asDigit)), trailingDigit , pf)
    }

    // Digits
    private def nonZeroDigit = satisfy(c => c.isDigit && c != '0').label("digit")
    private def nonZeroHexDigit = satisfy(c => isHexDigit(c) && c != '0').label("hexadecimal digit")
    private def nonZeroOctDigit = satisfy(c => isOctDigit(c) && c != '0').label("octal digit")
    private def nonZeroBit = '1'.label("bit")
    // why secret? so that the above digits can be marked as digits without "non-zero or zero digit" nonsense
    private def secretZero = '0'.hide #> BigInt(0)

    private def digit = character.digit
    private def hexDigit = character.hexDigit
    private def octDigit = character.octDigit
    private def bit = character.bit

    lazy val zeroAllowedDecimal = ofRadix(10, digit, err.labelIntegerDecimalEnd)
    lazy val zeroAllowedHexadecimal = ofRadix(16, hexDigit, err.labelIntegerHexadecimalEnd)
    lazy val zeroAllowedOctal = ofRadix(8, octDigit, err.labelIntegerOctalEnd)
    lazy val zeroAllowedBinary = ofRadix(2, bit, err.labelIntegerBinaryEnd)

    lazy val zeroNotAllowedDecimal = ofRadix(10, nonZeroDigit, digit, err.labelIntegerDecimalEnd) <|> secretZero
    lazy val zeroNotAllowedHexadecimal = ofRadix(16, nonZeroHexDigit, hexDigit, err.labelIntegerHexadecimalEnd) <|> secretZero
    lazy val zeroNotAllowedOctal = ofRadix(8, nonZeroOctDigit, octDigit, err.labelIntegerOctalEnd) <|> secretZero
    lazy val zeroNotAllowedBinary = ofRadix(2, nonZeroBit, bit, err.labelIntegerBinaryEnd) <|> secretZero

    def plainDecimal(desc: NumericDesc): Parsley[BigInt] = plainDecimal(desc.leadingZerosAllowed, desc.literalBreakChar)
    def plainDecimal(leadingZerosAllowed: Boolean, literalBreakChar: BreakCharDesc): Parsley[BigInt] = literalBreakChar match {
        case BreakCharDesc.NoBreakChar if leadingZerosAllowed     => zeroAllowedDecimal
        case BreakCharDesc.NoBreakChar                            => zeroNotAllowedDecimal
        case BreakCharDesc.Supported(c, _) if leadingZerosAllowed => ofRadix(10, digit, c, err.labelIntegerDecimalEnd)
        case BreakCharDesc.Supported(c, _)                        => ofRadix(10, nonZeroDigit, digit, c, err.labelIntegerDecimalEnd) <|> secretZero
    }

    def plainHexadecimal(desc: NumericDesc): Parsley[BigInt] = plainHexadecimal(desc.leadingZerosAllowed, desc.literalBreakChar)
    def plainHexadecimal(leadingZerosAllowed: Boolean, literalBreakChar: BreakCharDesc): Parsley[BigInt] = literalBreakChar match {
        case BreakCharDesc.NoBreakChar if leadingZerosAllowed     => zeroAllowedHexadecimal
        case BreakCharDesc.NoBreakChar                            => zeroNotAllowedHexadecimal
        case BreakCharDesc.Supported(c, _) if leadingZerosAllowed => ofRadix(16, hexDigit, c, err.labelIntegerHexadecimalEnd)
        case BreakCharDesc.Supported(c, _)                        => ofRadix(16, nonZeroHexDigit, hexDigit, c, err.labelIntegerHexadecimalEnd) <|> secretZero
    }

    def plainOctal(desc: NumericDesc): Parsley[BigInt] = plainOctal(desc.leadingZerosAllowed, desc.literalBreakChar)
    def plainOctal(leadingZerosAllowed: Boolean, literalBreakChar: BreakCharDesc): Parsley[BigInt] = literalBreakChar match {
        case BreakCharDesc.NoBreakChar if leadingZerosAllowed     => zeroAllowedOctal
        case BreakCharDesc.NoBreakChar                            => zeroNotAllowedOctal
        case BreakCharDesc.Supported(c, _) if leadingZerosAllowed => ofRadix(8, octDigit, c, err.labelIntegerOctalEnd)
        case BreakCharDesc.Supported(c, _)                        => ofRadix(8, nonZeroOctDigit, octDigit, c, err.labelIntegerOctalEnd) <|> secretZero
    }

    def plainBinary(desc: NumericDesc): Parsley[BigInt] = plainBinary(desc.leadingZerosAllowed, desc.literalBreakChar)
    def plainBinary(leadingZerosAllowed: Boolean, literalBreakChar: BreakCharDesc): Parsley[BigInt] = literalBreakChar match {
        case BreakCharDesc.NoBreakChar if leadingZerosAllowed     => zeroAllowedBinary
        case BreakCharDesc.NoBreakChar                            => zeroNotAllowedBinary
        case BreakCharDesc.Supported(c, _) if leadingZerosAllowed => ofRadix(2, bit, c, err.labelIntegerBinaryEnd)
        case BreakCharDesc.Supported(c, _)                        => ofRadix(2, nonZeroBit, bit, c, err.labelIntegerBinaryEnd) <|> secretZero
    }
}

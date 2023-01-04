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

    def zeroAllowedDecimal(endLabel: Option[String]) = ofRadix(10, digit, endLabel)
    def zeroAllowedHexadecimal(endLabel: Option[String]) = ofRadix(16, hexDigit, endLabel)
    def zeroAllowedOctal(endLabel: Option[String]) = ofRadix(8, octDigit, endLabel)
    def zeroAllowedBinary(endLabel: Option[String]) = ofRadix(2, bit, endLabel)

    def zeroNotAllowedDecimal(endLabel: Option[String]) = ofRadix(10, nonZeroDigit, digit, endLabel) <|> secretZero
    def zeroNotAllowedHexadecimal(endLabel: Option[String]) = ofRadix(16, nonZeroHexDigit, hexDigit, endLabel) <|> secretZero
    def zeroNotAllowedOctal(endLabel: Option[String]) = ofRadix(8, nonZeroOctDigit, octDigit, endLabel) <|> secretZero
    def zeroNotAllowedBinary(endLabel: Option[String]) = ofRadix(2, nonZeroBit, bit, endLabel) <|> secretZero

    def plainDecimal(desc: NumericDesc, endLabel: Option[String]): Parsley[BigInt] = plainDecimal(desc.leadingZerosAllowed, desc.literalBreakChar, endLabel)
    private def plainDecimal(leadingZerosAllowed: Boolean, literalBreakChar: BreakCharDesc, endLabel: Option[String]): Parsley[BigInt] = literalBreakChar match {
        case BreakCharDesc.NoBreakChar if leadingZerosAllowed     => zeroAllowedDecimal(endLabel)
        case BreakCharDesc.NoBreakChar                            => zeroNotAllowedDecimal(endLabel)
        case BreakCharDesc.Supported(c, _) if leadingZerosAllowed => ofRadix(10, digit, c, endLabel)
        case BreakCharDesc.Supported(c, _)                        => ofRadix(10, nonZeroDigit, digit, c, endLabel) <|> secretZero
    }

    def plainHexadecimal(desc: NumericDesc, endLabel: Option[String]): Parsley[BigInt] = plainHexadecimal(desc.leadingZerosAllowed, desc.literalBreakChar, endLabel)
    private def plainHexadecimal(leadingZerosAllowed: Boolean, literalBreakChar: BreakCharDesc, endLabel: Option[String]): Parsley[BigInt] = literalBreakChar match {
        case BreakCharDesc.NoBreakChar if leadingZerosAllowed     => zeroAllowedHexadecimal(endLabel)
        case BreakCharDesc.NoBreakChar                            => zeroNotAllowedHexadecimal(endLabel)
        case BreakCharDesc.Supported(c, _) if leadingZerosAllowed => ofRadix(16, hexDigit, c, endLabel)
        case BreakCharDesc.Supported(c, _)                        => ofRadix(16, nonZeroHexDigit, hexDigit, c, endLabel) <|> secretZero
    }

    def plainOctal(desc: NumericDesc, endLabel: Option[String]): Parsley[BigInt] = plainOctal(desc.leadingZerosAllowed, desc.literalBreakChar, endLabel)
    private def plainOctal(leadingZerosAllowed: Boolean, literalBreakChar: BreakCharDesc, endLabel: Option[String]): Parsley[BigInt] = literalBreakChar match {
        case BreakCharDesc.NoBreakChar if leadingZerosAllowed     => zeroAllowedOctal(endLabel)
        case BreakCharDesc.NoBreakChar                            => zeroNotAllowedOctal(endLabel)
        case BreakCharDesc.Supported(c, _) if leadingZerosAllowed => ofRadix(8, octDigit, c, endLabel)
        case BreakCharDesc.Supported(c, _)                        => ofRadix(8, nonZeroOctDigit, octDigit, c, endLabel) <|> secretZero
    }

    def plainBinary(desc: NumericDesc, endLabel: Option[String]): Parsley[BigInt] = plainBinary(desc.leadingZerosAllowed, desc.literalBreakChar, endLabel)
    private def plainBinary(leadingZerosAllowed: Boolean, literalBreakChar: BreakCharDesc, endLabel: Option[String]): Parsley[BigInt] = literalBreakChar match {
        case BreakCharDesc.NoBreakChar if leadingZerosAllowed     => zeroAllowedBinary(endLabel)
        case BreakCharDesc.NoBreakChar                            => zeroNotAllowedBinary(endLabel)
        case BreakCharDesc.Supported(c, _) if leadingZerosAllowed => ofRadix(2, bit, c, endLabel)
        case BreakCharDesc.Supported(c, _)                        => ofRadix(2, nonZeroBit, bit, c, endLabel) <|> secretZero
    }
}

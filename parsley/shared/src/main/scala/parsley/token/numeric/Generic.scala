/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.numeric

import parsley.Parsley, Parsley.pure
import parsley.character.{bit, digit, hexDigit, isHexDigit, isOctDigit, octDigit, satisfy}
import parsley.combinator.optional
import parsley.errors.combinator.ErrorMethods
import parsley.implicits.character.charLift
import parsley.token.descriptions.numeric.{BreakCharDesc, NumericDesc}

// These methods should not have labels on the leads, because that can interfere with their various uses
private [token] object Generic {
    private def ofRadix(radix: Int, digit: Parsley[Char]): Parsley[BigInt] = ofRadix(radix, digit, digit)
    private def ofRadix(radix: Int, startDigit: Parsley[Char], digit: Parsley[Char]): Parsley[BigInt] = {
        val pf = pure((x: BigInt, d: Char) => x*radix + d.asDigit)
        parsley.expr.infix.secretLeft1(startDigit.map(d => BigInt(d.asDigit)), digit, pf)
    }

    private def ofRadix(radix: Int, digit: Parsley[Char], breakChar: Char): Parsley[BigInt] = ofRadix(radix, digit, digit, breakChar)
    private def ofRadix(radix: Int, startDigit: Parsley[Char], digit: Parsley[Char], breakChar: Char): Parsley[BigInt] = {
        val pf = pure((x: BigInt, d: Char) => x*radix + d.asDigit)
        parsley.expr.infix.secretLeft1(startDigit.map(d => BigInt(d.asDigit)), optional(breakChar) *> digit, pf)
    }

    lazy val zeroAllowedDecimal = ofRadix(10, digit)
    lazy val zeroAllowedHexadecimal = ofRadix(16, hexDigit)
    lazy val zeroAllowedOctal = ofRadix(8, octDigit)
    lazy val zeroAllowedBinary = ofRadix(2, bit)

    private def nonZeroDigit = satisfy(c => c.isDigit && c != '0').label("digit")
    private def nonZeroHexDigit = satisfy(c => isHexDigit(c) && c != '0').label("hexadecimal digit")
    private def nonZeroOctDigit = satisfy(c => isOctDigit(c) && c != '0').label("octal digit")
    private def nonZeroBit = '1'.label("bit")
    // why secret? so that the above digits can be marked as digits without "non-zero or zero digit" nonsense
    private def secretZero = '0'.hide #> BigInt(0)

    lazy val zeroNotAllowedDecimal = ofRadix(10, nonZeroDigit, digit) <|> secretZero
    lazy val zeroNotAllowedHexadecimal = ofRadix(16, nonZeroHexDigit, hexDigit) <|> secretZero
    lazy val zeroNotAllowedOctal = ofRadix(8, nonZeroOctDigit, octDigit) <|> secretZero
    lazy val zeroNotAllowedBinary = ofRadix(2, nonZeroBit, bit) <|> secretZero

    def plainDecimal(desc: NumericDesc): Parsley[BigInt] = plainDecimal(desc.leadingZerosAllowed, desc.literalBreakChar)
    def plainDecimal(leadingZerosAllowed: Boolean, literalBreakChar: BreakCharDesc): Parsley[BigInt] = literalBreakChar match {
        case BreakCharDesc.NoBreakChar if leadingZerosAllowed     => zeroAllowedDecimal
        case BreakCharDesc.NoBreakChar                            => zeroNotAllowedDecimal
        case BreakCharDesc.Supported(c, _) if leadingZerosAllowed => ofRadix(10, digit, c)
        case BreakCharDesc.Supported(c, _)                        => ofRadix(10, nonZeroDigit, digit, c) <|> secretZero
    }

    def plainHexadecimal(desc: NumericDesc): Parsley[BigInt] = plainHexadecimal(desc.leadingZerosAllowed, desc.literalBreakChar)
    def plainHexadecimal(leadingZerosAllowed: Boolean, literalBreakChar: BreakCharDesc): Parsley[BigInt] = literalBreakChar match {
        case BreakCharDesc.NoBreakChar if leadingZerosAllowed     => zeroAllowedHexadecimal
        case BreakCharDesc.NoBreakChar                            => zeroNotAllowedHexadecimal
        case BreakCharDesc.Supported(c, _) if leadingZerosAllowed => ofRadix(16, hexDigit, c)
        case BreakCharDesc.Supported(c, _)                        => ofRadix(16, nonZeroHexDigit, hexDigit, c) <|> secretZero
    }

    def plainOctal(desc: NumericDesc): Parsley[BigInt] = plainOctal(desc.leadingZerosAllowed, desc.literalBreakChar)
    def plainOctal(leadingZerosAllowed: Boolean, literalBreakChar: BreakCharDesc): Parsley[BigInt] = literalBreakChar match {
        case BreakCharDesc.NoBreakChar if leadingZerosAllowed     => zeroAllowedOctal
        case BreakCharDesc.NoBreakChar                            => zeroNotAllowedOctal
        case BreakCharDesc.Supported(c, _) if leadingZerosAllowed => ofRadix(8, octDigit, c)
        case BreakCharDesc.Supported(c, _)                        => ofRadix(8, nonZeroOctDigit, octDigit, c) <|> secretZero
    }

    def plainBinary(desc: NumericDesc): Parsley[BigInt] = plainBinary(desc.leadingZerosAllowed, desc.literalBreakChar)
    def plainBinary(leadingZerosAllowed: Boolean, literalBreakChar: BreakCharDesc): Parsley[BigInt] = literalBreakChar match {
        case BreakCharDesc.NoBreakChar if leadingZerosAllowed     => zeroAllowedBinary
        case BreakCharDesc.NoBreakChar                            => zeroNotAllowedBinary
        case BreakCharDesc.Supported(c, _) if leadingZerosAllowed => ofRadix(2, bit, c)
        case BreakCharDesc.Supported(c, _)                        => ofRadix(2, nonZeroBit, bit, c) <|> secretZero
    }
}

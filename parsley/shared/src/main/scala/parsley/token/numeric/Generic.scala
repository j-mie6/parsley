/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.numeric

import parsley.Parsley, Parsley.pure
import parsley.character.{bit, digit, hexDigit, octDigit}
import parsley.combinator.optional
import parsley.extension.OperatorSugar
import parsley.implicits.character.charLift
import parsley.token.descriptions.numeric.{BreakCharDesc, NumericDesc}

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

    // TODO: these could improve by not using `-`
    lazy val zeroAllowedDecimal = ofRadix(10, digit)
    lazy val zeroAllowedHexadecimal = ofRadix(16, hexDigit)
    lazy val zeroAllowedOctal = ofRadix(8, octDigit)
    lazy val zeroAllowedBinary = ofRadix(2, bit)

    lazy val zeroNotAllowedDecimal = ofRadix(10, digit - '0', digit) <|> ('0' #> BigInt(0))
    lazy val zeroNotAllowedHexadecimal = ofRadix(16, hexDigit - '0', hexDigit) <|> ('0' #> BigInt(0))
    lazy val zeroNotAllowedOctal = ofRadix(8, octDigit - '0', octDigit) <|> ('0' #> BigInt(0))
    lazy val zeroNotAllowedBinary = ofRadix(2, '1', bit) <|> ('0' #> BigInt(0))

    def plainDecimal(desc: NumericDesc): Parsley[BigInt] = plainDecimal(desc.leadingZerosAllowed, desc.literalBreakChar)
    def plainDecimal(leadingZerosAllowed: Boolean, literalBreakChar: BreakCharDesc): Parsley[BigInt] = literalBreakChar match {
        case BreakCharDesc.NoBreakChar if leadingZerosAllowed     => zeroAllowedDecimal
        case BreakCharDesc.NoBreakChar                            => zeroNotAllowedDecimal
        case BreakCharDesc.Supported(c, _) if leadingZerosAllowed => ofRadix(10, digit, c)
        case BreakCharDesc.Supported(c, _)                        => ofRadix(10, digit - '0', digit, c) <|> ('0' #> BigInt(0))
    }

    def plainHexadecimal(desc: NumericDesc): Parsley[BigInt] = plainHexadecimal(desc.leadingZerosAllowed, desc.literalBreakChar)
    def plainHexadecimal(leadingZerosAllowed: Boolean, literalBreakChar: BreakCharDesc): Parsley[BigInt] = literalBreakChar match {
        case BreakCharDesc.NoBreakChar if leadingZerosAllowed     => zeroAllowedHexadecimal
        case BreakCharDesc.NoBreakChar                            => zeroNotAllowedHexadecimal
        case BreakCharDesc.Supported(c, _) if leadingZerosAllowed => ofRadix(16, hexDigit, c)
        case BreakCharDesc.Supported(c, _)                        => ofRadix(16, hexDigit - '0', hexDigit, c) <|> ('0' #> BigInt(0))
    }

    def plainOctal(desc: NumericDesc): Parsley[BigInt] = plainOctal(desc.leadingZerosAllowed, desc.literalBreakChar)
    def plainOctal(leadingZerosAllowed: Boolean, literalBreakChar: BreakCharDesc): Parsley[BigInt] = literalBreakChar match {
        case BreakCharDesc.NoBreakChar if leadingZerosAllowed     => zeroAllowedOctal
        case BreakCharDesc.NoBreakChar                            => zeroNotAllowedOctal
        case BreakCharDesc.Supported(c, _) if leadingZerosAllowed => ofRadix(8, octDigit, c)
        case BreakCharDesc.Supported(c, _)                        => ofRadix(8, octDigit - '0', octDigit, c) <|> ('0' #> BigInt(0))
    }

    def plainBinary(desc: NumericDesc): Parsley[BigInt] = plainBinary(desc.leadingZerosAllowed, desc.literalBreakChar)
    def plainBinary(leadingZerosAllowed: Boolean, literalBreakChar: BreakCharDesc): Parsley[BigInt] = literalBreakChar match {
        case BreakCharDesc.NoBreakChar if leadingZerosAllowed     => zeroAllowedBinary
        case BreakCharDesc.NoBreakChar                            => zeroNotAllowedBinary
        case BreakCharDesc.Supported(c, _) if leadingZerosAllowed => ofRadix(2, bit, c)
        case BreakCharDesc.Supported(c, _)                        => ofRadix(2, '1', bit, c) <|> ('0' #> BigInt(0))
    }
}

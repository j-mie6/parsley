/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.numeric

import parsley.Parsley, Parsley.pure
import parsley.character.{digit, hexDigit, octDigit, bit, oneOf}
import parsley.combinator.optional
import parsley.extension.OperatorSugar
import parsley.implicits.character.charLift
import parsley.token._
import parsley.token.descriptions.NumericDesc

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
    def plainDecimal(leadingZerosAllowed: Boolean, literalBreakChar: Option[Char]): Parsley[BigInt] = literalBreakChar match {
        case None if leadingZerosAllowed    => zeroAllowedDecimal
        case None                           => zeroNotAllowedDecimal
        case Some(c) if leadingZerosAllowed => ofRadix(10, digit, c)
        case Some(c)                        => ofRadix(10, digit - '0', digit, c)
    }

    def plainHexadecimal(desc: NumericDesc): Parsley[BigInt] = plainHexadecimal(desc.leadingZerosAllowed, desc.literalBreakChar)
    def plainHexadecimal(leadingZerosAllowed: Boolean, literalBreakChar: Option[Char]): Parsley[BigInt] = literalBreakChar match {
        case None if leadingZerosAllowed    => zeroAllowedHexadecimal
        case None                           => zeroNotAllowedHexadecimal
        case Some(c) if leadingZerosAllowed => ofRadix(16, hexDigit, c)
        case Some(c)                        => ofRadix(16, hexDigit - '0', hexDigit, c)
    }

    def plainOctal(desc: NumericDesc): Parsley[BigInt] = plainOctal(desc.leadingZerosAllowed, desc.literalBreakChar)
    def plainOctal(leadingZerosAllowed: Boolean, literalBreakChar: Option[Char]): Parsley[BigInt] = literalBreakChar match {
        case None if leadingZerosAllowed    => zeroAllowedOctal
        case None                           => zeroNotAllowedOctal
        case Some(c) if leadingZerosAllowed => ofRadix(8, octDigit, c)
        case Some(c)                        => ofRadix(8, octDigit - '0', octDigit, c)
    }

    def plainBinary(desc: NumericDesc): Parsley[BigInt] = plainBinary(desc.leadingZerosAllowed, desc.literalBreakChar)
    def plainBinary(leadingZerosAllowed: Boolean, literalBreakChar: Option[Char]): Parsley[BigInt] = literalBreakChar match {
        case None if leadingZerosAllowed    => zeroAllowedBinary
        case None                           => zeroNotAllowedBinary
        case Some(c) if leadingZerosAllowed => ofRadix(2, bit, c)
        case Some(c)                        => ofRadix(2, '1', bit, c)
    }
}

/* SPDX-FileCopyrightText: © 2021 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.numeric

import parsley.Parsley, Parsley.{attempt, empty, pure, unit}
import parsley.character.{digit, hexDigit, octDigit, oneOf}
import parsley.combinator, combinator.optional
import parsley.errors.combinator.{amend, entrench, ErrorMethods}
import parsley.implicits.character.charLift
import parsley.lift.lift2
import parsley.registers.Reg
import parsley.token.descriptions.numeric.{BreakCharDesc, ExponentDesc, NumericDesc}
import parsley.token.errors.ErrorConfig

private [token] final class UnsignedReal(desc: NumericDesc, natural: UnsignedInteger, err: ErrorConfig, generic: Generic) extends Real(err) {
    override lazy val decimal: Parsley[BigDecimal] = attempt(ofRadix(10, digit))
    override lazy val hexadecimal: Parsley[BigDecimal] = attempt('0' *> noZeroHexadecimal)
    override lazy val octal: Parsley[BigDecimal] = attempt('0' *> noZeroOctal)
    override lazy val binary: Parsley[BigDecimal] = attempt('0' *> noZeroBinary)
    override lazy val number: Parsley[BigDecimal] = {
        if (desc.decimalRealsOnly) decimal
        else {
            def addHex(p: Parsley[BigDecimal]) = {
                if (desc.realNumbersCanBeHexadecimal) noZeroHexadecimal <|> p
                else p
            }
            def addOct(p: Parsley[BigDecimal]) = {
                if (desc.realNumbersCanBeOctal) noZeroOctal <|> p
                else p
            }
            def addBin(p: Parsley[BigDecimal]) = {
                if (desc.realNumbersCanBeBinary) noZeroBinary <|> p
                else p
            }
            // this promotes sharing when the definitions would be otherwise equal
            val leadingDotAllowedDecimal = if (desc.leadingDotAllowed) decimal else ofRadix(10, digit, leadingDotAllowed = true)
            // not even accounting for the leading and trailing dot being allowed!
            val zeroLead = '0' *> (addHex(addOct(addBin(leadingDotAllowedDecimal <|> pure(BigDecimal(0))))))
            attempt(zeroLead <|> decimal)
        }
    }

    private def when(b: Boolean, p: Parsley[_]): Parsley[_] = if (b) p else unit

    val leadingBreakChar = desc.literalBreakChar match {
        case BreakCharDesc.NoBreakChar => unit
        case BreakCharDesc.Supported(breakChar, allowedAfterNonDecimalPrefix) => when(allowedAfterNonDecimalPrefix, optional(breakChar))
    }

    private val noZeroHexadecimal = when(desc.hexadecimalLeads.nonEmpty, oneOf(desc.hexadecimalLeads)) *> leadingBreakChar *> ofRadix(16, hexDigit)
    private val noZeroOctal = when(desc.octalLeads.nonEmpty, oneOf(desc.octalLeads)) *> leadingBreakChar *> ofRadix(8, octDigit)
    private val noZeroBinary = when(desc.binaryLeads.nonEmpty, oneOf(desc.binaryLeads)) *> leadingBreakChar *> ofRadix(2, oneOf('0', '1'))

    // could allow integers to be parsed here according to configuration, the intOrFloat captures that case anyway
    private def ofRadix(radix: Int, digit: Parsley[Char]): Parsley[BigDecimal] = ofRadix(radix, digit, desc.leadingDotAllowed)
    private def ofRadix(radix: Int, digit: Parsley[Char], leadingDotAllowed: Boolean): Parsley[BigDecimal] = {
        lazy val leadingHappened = Reg.make[Boolean]
        lazy val _noDoubleDroppedZero = leadingHappened.get.filterOut {
            case true => err.explainRealNoDoubleDroppedZero
        }
        val expDesc = desc.exponentDescForRadix(radix)
        val whole = radix match {
            case 10 => generic.plainDecimal(desc, None)
            case 16 => generic.plainHexadecimal(desc, None)
            case 8 => generic.plainOctal(desc, None)
            case 2 => generic.plainBinary(desc, None)
        }
        val f = (d: Char, x: BigDecimal) => x/radix + d.asDigit
        def broken(c: Char) = lift2(f, digit, (optional(c) *> digit).foldRight[BigDecimal](0)(f))
        val fractional = amend {
            '.' *> {
                desc.literalBreakChar match {
                    case BreakCharDesc.NoBreakChar if desc.trailingDotAllowed     =>
                        if (!leadingDotAllowed) entrench(digit.foldRight[BigDecimal](0)(f))
                        else entrench(digit.foldRight1[BigDecimal](0)(f)) <|> _noDoubleDroppedZero *> pure[BigDecimal](0)
                    case BreakCharDesc.NoBreakChar                                => entrench(digit.foldRight1[BigDecimal](0)(f))
                    case BreakCharDesc.Supported(c, _) if desc.trailingDotAllowed =>
                        entrench(broken(c)) <|> when(leadingDotAllowed, _noDoubleDroppedZero) *> pure[BigDecimal](0)
                    case BreakCharDesc.Supported(c, _)                            => entrench(broken(c))
                }
            }
        }
        val (requiredExponent, exponent, base) = expDesc match {
            case ExponentDesc.Supported(compulsory, exp, base, sign) =>
                val expErr = new ErrorConfig {
                    override def pleaseDontValidateConfig: Boolean = true
                    override def labelIntegerSignedDecimal(bits: Int): Option[String] = None
                    override def labelIntegerBinaryEnd: Option[String] = None
                }
                val integer = new SignedInteger(desc.copy(positiveSign = sign), natural, expErr)
                val exponent = oneOf(exp) *> integer.decimal32
                if (compulsory) (exponent, exponent, base)
                else (exponent, exponent <|> pure(0), base)
            // this can't fail for non-required, it has to be the identity exponent
            case ExponentDesc.NoExponents => (empty, pure(0), 1)
        }
        val fractExponent =
                (lift2((f: BigDecimal, e: Int) => (w: BigInt) => (BigDecimal(w) + f / radix) * BigDecimal(base).pow(e),
                       fractional,
                       exponent)
            <|> requiredExponent.map(e => (w: BigInt) => BigDecimal(w) * BigDecimal(base).pow(e)))
        val configuredWhole =
            if (leadingDotAllowed) (
                    when(desc.trailingDotAllowed, leadingHappened.put(false)) *> whole
                <|> when(desc.trailingDotAllowed, leadingHappened.put(true)) *> pure(BigInt(0))
            )
            else whole
        configuredWhole <**> fractExponent
    }
}

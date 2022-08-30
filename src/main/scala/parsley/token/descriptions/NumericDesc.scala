/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.descriptions

import parsley.token.{Impl, NotRequired}
import parsley.token.numeric.Rational

private [token] // TODO: Remove
sealed abstract class Presence
private [token] // TODO: Remove
object Presence {
    case object Required extends Presence
    case object Optional extends Presence
    case object Illegal extends Presence
}

private [token] // TODO: Remove
sealed abstract class ExponentDesc
private [token] // TODO: Remove
object ExponentDesc {
    case object NoExponents extends ExponentDesc
    case class Supported(compulsory: Boolean, chars: Set[Char], base: Int, positiveSign: Presence) extends ExponentDesc
}

private [token] // TODO: Remove
case class NumericDesc (literalBreakChar: Option[Char],
                        leadingDotAllowed: Boolean,
                        trailingDotAllowed: Boolean,
                        leadingZerosAllowed: Boolean,
                        positiveSign: Presence,
                        // generic number
                        integerNumbersCanBeHexadecimal: Boolean,
                        integerNumbersCanBeOctal: Boolean,
                        integerNumbersCanBeBinary: Boolean,
                        rationalNumbersCanBeHexadecimal: Boolean,
                        rationalNumbersCanBeOctal: Boolean,
                        rationalNumbersCanBeBinary: Boolean,
                        // special literals
                        hexadecimalLeads: Set[Char],
                        octalLeads: Set[Char],
                        binaryLeads: Set[Char],
                        // exponents
                        decimalExponentDesc: ExponentDesc,
                        hexadecimalExponentDesc: ExponentDesc,
                        octalExponentDesc: ExponentDesc,
                        binaryExponentDesc: ExponentDesc
                       ) {
    private [token] def leadsForRadix(x: Int): Set[Char] = (x: @unchecked) match {
        case 10 => Set.empty
        case 16 => hexadecimalLeads
        case 8 => octalLeads
        case 2 => binaryLeads
    }

    private [token] def exponentDescForRadix(x: Int): ExponentDesc = (x: @unchecked) match {
        case 10 => decimalExponentDesc
        case 16 => hexadecimalExponentDesc
        case 8 => octalExponentDesc
        case 2 => binaryExponentDesc
    }

    private [token] def radixAllowedForInteger(x: Int): Boolean = (x: @unchecked) match {
        case 10 => true
        case 16 => integerNumbersCanBeHexadecimal
        case 8 => integerNumbersCanBeOctal
        case 2 => integerNumbersCanBeBinary
    }

    private [token] def radixAllowedForRational(x: Int): Boolean = (x: @unchecked) match {
        case 10 => true
        case 16 => rationalNumbersCanBeHexadecimal
        case 8 => rationalNumbersCanBeOctal
        case 2 => rationalNumbersCanBeBinary
    }
}

private [token] // TODO: Remove
object NumericDesc {
    val plain: NumericDesc = NumericDesc(
        literalBreakChar = None,
        leadingDotAllowed = false,
        trailingDotAllowed = false,
        leadingZerosAllowed = true,
        positiveSign = Presence.Optional,
        // generic number
        integerNumbersCanBeHexadecimal = true,
        integerNumbersCanBeOctal = true,
        integerNumbersCanBeBinary = false,
        rationalNumbersCanBeHexadecimal = false,
        rationalNumbersCanBeOctal = false,
        rationalNumbersCanBeBinary = false,
        // special literals
        hexadecimalLeads = Set('x', 'X'),
        octalLeads = Set('o', 'O'),
        binaryLeads = Set('b', 'B'),
        // exponents
        decimalExponentDesc = ExponentDesc.Supported(compulsory = false, chars = Set('e', 'E'), base = 10, positiveSign = Presence.Optional),
        hexadecimalExponentDesc = ExponentDesc.Supported(compulsory = true, chars = Set('p', 'P'), base = 2, positiveSign = Presence.Optional),
        octalExponentDesc = ExponentDesc.Supported(compulsory = true, chars = Set('e', 'E', 'p', 'P'), base = 2, positiveSign = Presence.Optional),
        binaryExponentDesc = ExponentDesc.Supported(compulsory = true, chars = Set('e', 'E', 'p', 'P'), base = 2, positiveSign = Presence.Optional)
    )
}

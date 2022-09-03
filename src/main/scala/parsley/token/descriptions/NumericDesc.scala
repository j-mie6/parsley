/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.descriptions.numeric

import parsley.token.{Impl, NotRequired}
import parsley.token.numeric.Real

sealed abstract class PlusSignPresence
object PlusSignPresence {
    case object Required extends PlusSignPresence
    case object Optional extends PlusSignPresence
    case object Illegal extends PlusSignPresence
}

sealed abstract class ExponentDesc
object ExponentDesc {
    case object NoExponents extends ExponentDesc
    case class Supported(compulsory: Boolean,
                         chars: Set[Char],
                         base: Int,
                         positiveSign: PlusSignPresence
                        ) extends ExponentDesc {
        if (chars.isEmpty) throw new IllegalArgumentException("The characters used for floating point exponents must not be empty")
    }
}

sealed abstract class BreakCharDesc
object BreakCharDesc {
    case object NoBreakChar extends BreakCharDesc
    case class Supported(breakChar: Char, allowedAfterNonDecimalPrefix: Boolean) extends BreakCharDesc
}

case class NumericDesc (literalBreakChar: BreakCharDesc,
                        leadingDotAllowed: Boolean,
                        trailingDotAllowed: Boolean,
                        leadingZerosAllowed: Boolean,
                        positiveSign: PlusSignPresence,
                        // generic number
                        integerNumbersCanBeHexadecimal: Boolean,
                        integerNumbersCanBeOctal: Boolean,
                        integerNumbersCanBeBinary: Boolean,
                        realNumbersCanBeHexadecimal: Boolean,
                        realNumbersCanBeOctal: Boolean,
                        realNumbersCanBeBinary: Boolean,
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
    // These aren't necessarily the case, C octals, for instance, are of the form 010!
    //if (hexadecimalLeads.isEmpty) throw new IllegalArgumentException("The leading characters of hexadecimal literals must not be empty")
    //if (octalLeads.isEmpty) throw new IllegalArgumentException("The leading characters of octal literals must not be empty")
    //if (binaryLeads.isEmpty) throw new IllegalArgumentException("The leading characters of binary literals must not be empty")

    private [token] def exponentDescForRadix(x: Int): ExponentDesc = (x: @unchecked) match {
        case 10 => decimalExponentDesc
        case 16 => hexadecimalExponentDesc
        case 8 => octalExponentDesc
        case 2 => binaryExponentDesc
    }

    private [token] def decimalIntegersOnly: Boolean = !(integerNumbersCanBeBinary || integerNumbersCanBeHexadecimal || integerNumbersCanBeOctal)
    private [token] def decimalRealsOnly: Boolean = !(realNumbersCanBeBinary || realNumbersCanBeHexadecimal || realNumbersCanBeOctal)
}

object NumericDesc {
    val plain: NumericDesc = NumericDesc(
        literalBreakChar = BreakCharDesc.NoBreakChar,
        leadingDotAllowed = false,
        trailingDotAllowed = false,
        leadingZerosAllowed = true,
        positiveSign = PlusSignPresence.Optional,
        // generic number
        integerNumbersCanBeHexadecimal = true,
        integerNumbersCanBeOctal = true,
        integerNumbersCanBeBinary = false,
        realNumbersCanBeHexadecimal = false,
        realNumbersCanBeOctal = false,
        realNumbersCanBeBinary = false,
        // special literals
        hexadecimalLeads = Set('x', 'X'),
        octalLeads = Set('o', 'O'),
        binaryLeads = Set('b', 'B'),
        // exponents
        decimalExponentDesc = ExponentDesc.Supported(compulsory = false, chars = Set('e', 'E'), base = 10, positiveSign = PlusSignPresence.Optional),
        hexadecimalExponentDesc = ExponentDesc.Supported(compulsory = true, chars = Set('p', 'P'), base = 2, positiveSign = PlusSignPresence.Optional),
        octalExponentDesc = ExponentDesc.Supported(compulsory = true, chars = Set('e', 'E', 'p', 'P'), base = 2, positiveSign = PlusSignPresence.Optional),
        binaryExponentDesc = ExponentDesc.Supported(compulsory = true, chars = Set('e', 'E', 'p', 'P'), base = 2, positiveSign = PlusSignPresence.Optional)
    )
}

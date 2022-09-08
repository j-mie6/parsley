/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.descriptions.numeric

import parsley.token.{Impl, NotRequired}
import parsley.token.numeric.Real

/** TODO:
  *
  * @since 4.0.0
  */
sealed abstract class PlusSignPresence
/** TODO:
  *
  * @since 4.0.0
  */
object PlusSignPresence {
    /** TODO:
      *
      * @since 4.0.0
      */
    case object Required extends PlusSignPresence
    /** TODO:
      *
      * @since 4.0.0
      */
    case object Optional extends PlusSignPresence
    /** TODO:
      *
      * @since 4.0.0
      */
    case object Illegal extends PlusSignPresence
}

/** TODO:
  *
  * @since 4.0.0
  */
sealed abstract class ExponentDesc
/** TODO:
  *
  * @since 4.0.0
  */
object ExponentDesc {
    /** TODO:
      *
      * @since 4.0.0
      */
    case object NoExponents extends ExponentDesc
    /** TODO:
      *
      * @param compulsory
      * @param chars
      * @param base
      * @param positiveSign
      * @since 4.0.0
      */
    case class Supported(compulsory: Boolean,
                         chars: Set[Char],
                         base: Int,
                         positiveSign: PlusSignPresence
                        ) extends ExponentDesc {
        if (chars.isEmpty) throw new IllegalArgumentException("The characters used for floating point exponents must not be empty")
    }
}

/** TODO:
  *
  * @since 4.0.0
  */
sealed abstract class BreakCharDesc
/** TODO:
  *
  * @since 4.0.0
  */
object BreakCharDesc {
    /** TODO:
      *
      * @since 4.0.0
      */
    case object NoBreakChar extends BreakCharDesc
    /** TODO:
      *
      * @param breakChar
      * @param allowedAfterNonDecimalPrefix
      * @since 4.0.0
      */
    case class Supported(breakChar: Char, allowedAfterNonDecimalPrefix: Boolean) extends BreakCharDesc
}

/** TODO:
  *
  * @param literalBreakChar
  * @param leadingDotAllowed
  * @param trailingDotAllowed
  * @param leadingZerosAllowed
  * @param positiveSign
  * @param integerNumbersCanBeHexadecimal
  * @param integerNumbersCanBeOctal
  * @param integerNumbersCanBeBinary
  * @param realNumbersCanBeHexadecimal
  * @param realNumbersCanBeOctal
  * @param realNumbersCanBeBinary
  * @param hexadecimalLeads
  * @param octalLeads
  * @param binaryLeads
  * @param decimalExponentDesc
  * @param hexadecimalExponentDesc
  * @param octalExponentDesc
  * @param binaryExponentDesc
  * @since 4.0.0
  */
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
    private [token] def exponentDescForRadix(x: Int): ExponentDesc = (x: @unchecked) match {
        case 10 => decimalExponentDesc
        case 16 => hexadecimalExponentDesc
        case 8 => octalExponentDesc
        case 2 => binaryExponentDesc
    }

    private [token] def decimalIntegersOnly: Boolean = !(integerNumbersCanBeBinary || integerNumbersCanBeHexadecimal || integerNumbersCanBeOctal)
    private [token] def decimalRealsOnly: Boolean = !(realNumbersCanBeBinary || realNumbersCanBeHexadecimal || realNumbersCanBeOctal)
}

/** TODO:
  *
  * @since 4.0.0
  */
object NumericDesc {
    /** TODO:
      *
      * @since 4.0.0
      */
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

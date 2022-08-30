/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.descriptions

import parsley.token.{Impl, NotRequired}

sealed abstract class Presence
object Presence {
    case object Required extends Presence
    case object Optional extends Presence
    case object Illegal extends Presence
}

sealed abstract class RationalDesc
object RationalDesc {
    case object Illegal extends RationalDesc
    case class Supported()
}

private [token]
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
                       ) {

}

private [token]
object NumericDesc {
    val plain: NumericDesc = null
    //val plain = NumericDesc()
}

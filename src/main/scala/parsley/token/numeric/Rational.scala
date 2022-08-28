/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.numeric

import parsley.Parsley
import parsley.errors.combinator.ErrorMethods
import parsley.token._

abstract class Rational private[token] {
    def decimal: Parsley[BigDecimal]
    def hexadecimal: Parsley[BigDecimal]
    def octal: Parsley[BigDecimal]
    def binary: Parsley[BigDecimal]
    def number: Parsley[BigDecimal]

    final def decimalFloatRounded: Parsley[Float] = decimal.map(_.toFloat)
    final def hexadecimalFloatRounded: Parsley[Float] = hexadecimal.map(_.toFloat)
    final def octalFloatRounded: Parsley[Float] = octal.map(_.toFloat)
    final def binaryFloatRounded: Parsley[Float] = binary.map(_.toFloat)
    final def floatRounded: Parsley[Float] = number.map(_.toFloat)

    final def decimalDoubleRounded: Parsley[Double] = decimal.map(_.toDouble)
    final def hexadecimalDoubleRounded: Parsley[Double] = hexadecimal.map(_.toDouble)
    final def octalDoubleRounded: Parsley[Double] = octal.map(_.toDouble)
    final def binaryDoubleRounded: Parsley[Double] = binary.map(_.toDouble)
    final def doubleRounded: Parsley[Double] = number.map(_.toDouble)

    lazy val decimalFloat: Parsley[Float] = ensureFloat(_decimal)
    lazy val hexadecimalFloat: Parsley[Float] = ensureFloat(_hexadecimal)
    lazy val octalFloat: Parsley[Float] = ensureFloat(_octal)
    lazy val binaryFloat: Parsley[Float] = ensureFloat(_binary)
    lazy val float: Parsley[Float] = ensureFloat(_number)

    lazy val decimalDouble: Parsley[Double] = ensureDouble(_decimal)
    lazy val hexadecimalDouble: Parsley[Double] = ensureDouble(_hexadecimal)
    lazy val octalDouble: Parsley[Double] = ensureDouble(_octal)
    lazy val binaryDouble: Parsley[Double] = ensureDouble(_binary)
    lazy val double: Parsley[Double] = ensureDouble(_number)

    protected [numeric] def ensureFloat(number: Parsley[BigDecimal]): Parsley[Float] =
        number.collectMsg(n => Seq(s"$n cannot be represented exactly as a IEEE 754 single-precision float")) {
            case n if n.isBinaryFloat || n.isDecimalFloat || n.isExactFloat => n.toFloat
        }

    protected [numeric] def ensureDouble(number: Parsley[BigDecimal]): Parsley[Double] =
        number.collectMsg(n => Seq(s"$n cannot be represented exactly as a IEEE 754 double-precision float")) {
            case n if n.isBinaryDouble || n.isDecimalDouble || n.isExactDouble => n.toDouble
        }

    protected [numeric] def _decimal: Parsley[BigDecimal] = decimal
    protected [numeric] def _hexadecimal: Parsley[BigDecimal] = hexadecimal
    protected [numeric] def _octal: Parsley[BigDecimal] = octal
    protected [numeric] def _binary: Parsley[BigDecimal] = binary
    protected [numeric] def _number: Parsley[BigDecimal] = number
}

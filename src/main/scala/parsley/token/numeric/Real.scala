/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.numeric

import parsley.Parsley
import parsley.errors.combinator.{amend, entrench, ErrorMethods}

/** This class defines a uniform interface for defining parsers for floating
  * literals, independent of how whitespace should be handled after the literal.
  *
  * @since 4.0.0
  * @note implementations of this class found within `Lexer` may employ sharing
  *       and refine the non-final `def`s in this class into `val` or `lazy val` when overriding.
  *
  * @define disclaimer
  *   the exact behaviour of this parser is decided by the implementations given in
  *   `Lexer`, which will depend on user-defined configuration. Please see the
  *   relevant documentation of these specific objects.
  */
abstract class Real private[token] {
    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    def decimal: Parsley[BigDecimal]
    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    def hexadecimal: Parsley[BigDecimal]
    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    def octal: Parsley[BigDecimal]
    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    def binary: Parsley[BigDecimal]
    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    def number: Parsley[BigDecimal]

    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    final def decimalFloatRounded: Parsley[Float] = decimal.map(_.toFloat)
    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    final def hexadecimalFloatRounded: Parsley[Float] = hexadecimal.map(_.toFloat)
    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    final def octalFloatRounded: Parsley[Float] = octal.map(_.toFloat)
    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    final def binaryFloatRounded: Parsley[Float] = binary.map(_.toFloat)
    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    final def floatRounded: Parsley[Float] = number.map(_.toFloat)

    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    final def decimalDoubleRounded: Parsley[Double] = decimal.map(_.toDouble)
    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    final def hexadecimalDoubleRounded: Parsley[Double] = hexadecimal.map(_.toDouble)
    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    final def octalDoubleRounded: Parsley[Double] = octal.map(_.toDouble)
    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    final def binaryDoubleRounded: Parsley[Double] = binary.map(_.toDouble)
    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    final def doubleRounded: Parsley[Double] = number.map(_.toDouble)

    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    lazy val decimalFloat: Parsley[Float] = ensureFloat(_decimal)
    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    lazy val hexadecimalFloat: Parsley[Float] = ensureFloat(_hexadecimal)
    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    lazy val octalFloat: Parsley[Float] = ensureFloat(_octal)
    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    lazy val binaryFloat: Parsley[Float] = ensureFloat(_binary)
    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    lazy val float: Parsley[Float] = ensureFloat(_number)

    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    lazy val decimalDouble: Parsley[Double] = ensureDouble(_decimal)
    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    lazy val hexadecimalDouble: Parsley[Double] = ensureDouble(_hexadecimal)
    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    lazy val octalDouble: Parsley[Double] = ensureDouble(_octal)
    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    lazy val binaryDouble: Parsley[Double] = ensureDouble(_binary)
    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    lazy val double: Parsley[Double] = ensureDouble(_number)

    protected [numeric] def ensureFloat(number: Parsley[BigDecimal]): Parsley[Float] = amend {
        entrench(number).collectMsg(n => Seq(s"$n cannot be represented exactly as a IEEE 754 single-precision float")) {
            case n if n.isBinaryFloat || n.isDecimalFloat || n.isExactFloat => n.toFloat
        }
    }

    protected [numeric] def ensureDouble(number: Parsley[BigDecimal]): Parsley[Double] = amend {
        entrench(number).collectMsg(n => Seq(s"$n cannot be represented exactly as a IEEE 754 double-precision float")) {
            case n if n.isBinaryDouble || n.isDecimalDouble || n.isExactDouble => n.toDouble
        }
    }

    protected [numeric] def _decimal: Parsley[BigDecimal] = decimal
    protected [numeric] def _hexadecimal: Parsley[BigDecimal] = hexadecimal
    protected [numeric] def _octal: Parsley[BigDecimal] = octal
    protected [numeric] def _binary: Parsley[BigDecimal] = binary
    protected [numeric] def _number: Parsley[BigDecimal] = number
}

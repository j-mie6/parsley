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

    // $COVERAGE-OFF$
    // It's not so important these are tested, they are just wrappers around the bottom ones
    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    @inline final def decimalFloatRounded: Parsley[Float] = decimal.map(_.toFloat)
    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    @inline final def hexadecimalFloatRounded: Parsley[Float] = hexadecimal.map(_.toFloat)
    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    @inline final def octalFloatRounded: Parsley[Float] = octal.map(_.toFloat)
    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    @inline final def binaryFloatRounded: Parsley[Float] = binary.map(_.toFloat)
    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    @inline final def floatRounded: Parsley[Float] = number.map(_.toFloat)

    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    @inline final def decimalDoubleRounded: Parsley[Double] = decimal.map(_.toDouble)
    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    @inline final def hexadecimalDoubleRounded: Parsley[Double] = hexadecimal.map(_.toDouble)
    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    @inline final def octalDoubleRounded: Parsley[Double] = octal.map(_.toDouble)
    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    @inline final def binaryDoubleRounded: Parsley[Double] = binary.map(_.toDouble)
    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    @inline final def doubleRounded: Parsley[Double] = number.map(_.toDouble)

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
    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    lazy val decimalExactFloat: Parsley[Float] = ensureExactFloat(_decimal)
    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    lazy val hexadecimalExactFloat: Parsley[Float] = ensureExactFloat(_hexadecimal)
    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    lazy val octalExactFloat: Parsley[Float] = ensureExactFloat(_octal)
    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    lazy val binaryExactFloat: Parsley[Float] = ensureExactFloat(_binary)
    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    lazy val exactFloat: Parsley[Float] = ensureExactFloat(_number)

    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    lazy val decimalExactDouble: Parsley[Double] = ensureExactDouble(_decimal)
    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    lazy val hexadecimalExactDouble: Parsley[Double] = ensureExactDouble(_hexadecimal)
    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    lazy val octalExactDouble: Parsley[Double] = ensureExactDouble(_octal)
    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    lazy val binaryExactDouble: Parsley[Double] = ensureExactDouble(_binary)
    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    lazy val exactDouble: Parsley[Double] = ensureExactDouble(_number)
    // $COVERAGE-ON$

    protected [numeric] def ensureFloat(number: Parsley[BigDecimal]): Parsley[Float] = amend {
        entrench(number).collectMsg(n => Seq(if (n > BigDecimal(Float.MaxValue) || n < BigDecimal(Float.MinValue)) s"literal $n is too large to be an IEEE 754 single-precision float"
                                             else                                                                  s"literal $n is too small to be an IEEE 754 single-precision float")) {
            case n if isFloat(n) => n.toFloat
        }
    }

    private def isFloat(n: BigDecimal): Boolean = {
        n == 0.0 || n == -0.0 || {
            val x = n.toFloat
            x.isFinite && x != 0.0 && x != -0.0
        }
    }

    protected [numeric] def ensureDouble(number: Parsley[BigDecimal]): Parsley[Double] = amend {
        entrench(number).collectMsg(n => Seq(if (n > BigDecimal(Double.MaxValue) || n < BigDecimal(Double.MinValue)) s"literal $n is too large to be an IEEE 754 double-precision float"
                                             else                                                                    s"literal $n is too small to be an IEEE 754 double-precision float")) {
            case n if isDouble(n) => n.toDouble
        }
    }

    private def isDouble(n: BigDecimal): Boolean = {
        n == 0.0 || n == -0.0 || {
            val x = n.toDouble
            x.isFinite && x != 0.0 && x != -0.0
        }
    }

    protected [numeric] def ensureExactFloat(number: Parsley[BigDecimal]): Parsley[Float] = amend {
        entrench(number).collectMsg(n => Seq(s"$n cannot be represented exactly as a IEEE 754 single-precision float")) {
            case n if n.isExactFloat => n.toFloat
        }
    }

    protected [numeric] def ensureExactDouble(number: Parsley[BigDecimal]): Parsley[Double] = amend {
        entrench(number).collectMsg(n => Seq(s"$n cannot be represented exactly as a IEEE 754 double-precision float")) {
            case n if n.isExactDouble => n.toDouble
        }
    }

    // $COVERAGE-OFF$
    protected [numeric] def _decimal: Parsley[BigDecimal] = decimal
    protected [numeric] def _hexadecimal: Parsley[BigDecimal] = hexadecimal
    protected [numeric] def _octal: Parsley[BigDecimal] = octal
    protected [numeric] def _binary: Parsley[BigDecimal] = binary
    protected [numeric] def _number: Parsley[BigDecimal] = number
    // $COVERAGE-ON$
}

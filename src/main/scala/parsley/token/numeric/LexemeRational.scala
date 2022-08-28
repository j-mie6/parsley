/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.numeric

import parsley.Parsley
import parsley.token.{Bits, CanHold}

private [token] final class LexemeRational(rational: Rational, ws: Parsley[_]) extends Rational {
    override def decimal: Parsley[BigDecimal] = lexeme(rational.decimal)
    override def hexadecimal: Parsley[BigDecimal] = lexeme(rational.hexadecimal)
    override def octal: Parsley[BigDecimal] = lexeme(rational.octal)
    override def binary: Parsley[BigDecimal] = lexeme(rational.binary)
    override def number: Parsley[BigDecimal] = lexeme(rational.number)

    override protected def _decimal: Parsley[BigDecimal] = rational.decimal
    override protected def _hexadecimal: Parsley[BigDecimal] = rational.hexadecimal
    override protected def _octal: Parsley[BigDecimal] = rational.octal
    override protected def _binary: Parsley[BigDecimal] = rational.binary
    override protected def _number: Parsley[BigDecimal] = rational.number

    override protected def ensureFloat(number: Parsley[BigDecimal]): Parsley[Float] = lexeme(super.ensureFloat(number))
    override protected def ensureDouble(number: Parsley[BigDecimal]): Parsley[Double] = lexeme(super.ensureDouble(number))

    private def lexeme[A](p: Parsley[A]) = p <* ws
}

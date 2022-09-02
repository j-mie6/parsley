/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.numeric

import parsley.Parsley

private [token] final class LexemeReal(rational: Real, ws: Parsley[_]) extends Real {
    override def decimal: Parsley[BigDecimal] = lexeme(rational.decimal)
    override def hexadecimal: Parsley[BigDecimal] = lexeme(rational.hexadecimal)
    override def octal: Parsley[BigDecimal] = lexeme(rational.octal)
    override def binary: Parsley[BigDecimal] = lexeme(rational.binary)
    override def number: Parsley[BigDecimal] = lexeme(rational.number)

    override protected [numeric] def _decimal: Parsley[BigDecimal] = rational.decimal
    override protected [numeric] def _hexadecimal: Parsley[BigDecimal] = rational.hexadecimal
    override protected [numeric] def _octal: Parsley[BigDecimal] = rational.octal
    override protected [numeric] def _binary: Parsley[BigDecimal] = rational.binary
    override protected [numeric] def _number: Parsley[BigDecimal] = rational.number

    override protected [numeric] def ensureFloat(number: Parsley[BigDecimal]): Parsley[Float] = lexeme(super.ensureFloat(number))
    override protected [numeric] def ensureDouble(number: Parsley[BigDecimal]): Parsley[Double] = lexeme(super.ensureDouble(number))

    private def lexeme[A](p: Parsley[A]) = p <* ws
}

/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.numeric

import parsley.Parsley
import parsley.token.Lexeme

private [token] final class LexemeReal(rational: Real, lexeme: Lexeme) extends Real {
    override lazy val decimal: Parsley[BigDecimal] = lexeme(rational.decimal)
    override lazy val hexadecimal: Parsley[BigDecimal] = lexeme(rational.hexadecimal)
    override lazy val octal: Parsley[BigDecimal] = lexeme(rational.octal)
    override lazy val binary: Parsley[BigDecimal] = lexeme(rational.binary)
    override lazy val number: Parsley[BigDecimal] = lexeme(rational.number)

    override protected [numeric] def _decimal: Parsley[BigDecimal] = rational.decimal
    override protected [numeric] def _hexadecimal: Parsley[BigDecimal] = rational.hexadecimal
    override protected [numeric] def _octal: Parsley[BigDecimal] = rational.octal
    override protected [numeric] def _binary: Parsley[BigDecimal] = rational.binary
    override protected [numeric] def _number: Parsley[BigDecimal] = rational.number

    override protected [numeric] def ensureFloat(number: Parsley[BigDecimal]): Parsley[Float] = lexeme(super.ensureFloat(number))
    override protected [numeric] def ensureDouble(number: Parsley[BigDecimal]): Parsley[Double] = lexeme(super.ensureDouble(number))
    override protected [numeric] def ensureExactFloat(number: Parsley[BigDecimal]): Parsley[Float] = lexeme(super.ensureExactFloat(number))
    override protected [numeric] def ensureExactDouble(number: Parsley[BigDecimal]): Parsley[Double] = lexeme(super.ensureExactDouble(number))
}

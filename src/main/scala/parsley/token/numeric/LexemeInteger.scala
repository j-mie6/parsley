/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.numeric

import parsley.Parsley
import parsley.token.{Bits, CanHold}

private [token] final class LexemeInteger(integer: Integer, ws: Parsley[_]) extends Integer {
    override private [numeric]  val desc = integer.desc
    override lazy val decimal: Parsley[BigInt] = lexeme(integer.decimal)
    override lazy val hexadecimal: Parsley[BigInt] = lexeme(integer.hexadecimal)
    override lazy val octal: Parsley[BigInt] = lexeme(integer.octal)
    override lazy val binary: Parsley[BigInt] = lexeme(integer.binary)
    override lazy val  number: Parsley[BigInt] = lexeme(integer.number)

    override protected[numeric] def bounded[T](number: Parsley[BigInt], bits: Bits, radix: Int)(implicit ev: CanHold[bits.self,T]): Parsley[T] =
        lexeme(integer.bounded(number, bits, radix))

    override protected [numeric] def _decimal: Parsley[BigInt] = integer.decimal
    override protected [numeric] def _hexadecimal: Parsley[BigInt] = integer.hexadecimal
    override protected [numeric] def _octal: Parsley[BigInt] = integer.octal
    override protected [numeric] def _binary: Parsley[BigInt] = integer.binary
    override protected [numeric] def _number: Parsley[BigInt] = integer.number

    private def lexeme[A](p: Parsley[A]) = p <* ws
}

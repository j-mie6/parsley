/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.numeric

import parsley.Parsley, Parsley.pure
import parsley.character.{digit, hexDigit, octDigit, oneOf}
import parsley.combinator.optional
import parsley.extension.OperatorSugar
import parsley.implicits.character.charLift
import parsley.token._
import parsley.token.descriptions.numeric.NumericDesc

abstract class Integer private[token] (private [numeric] val desc: NumericDesc) {
    def decimal: Parsley[BigInt]
    def hexadecimal: Parsley[BigInt]
    def octal: Parsley[BigInt]
    def binary: Parsley[BigInt]
    def number: Parsley[BigInt]

    final def number8[T: CanHold.can_hold_8_bits]: Parsley[T] = numberBounded(_8)
    final def decimal8[T: CanHold.can_hold_8_bits]: Parsley[T] = decimalBounded(_8)
    final def hexadecimal8[T: CanHold.can_hold_8_bits]: Parsley[T] = hexadecimalBounded(_8)
    final def octal8[T: CanHold.can_hold_8_bits]: Parsley[T] = octalBounded(_8)
    final def binary8[T: CanHold.can_hold_8_bits]: Parsley[T] = binaryBounded(_8)

    final def number16[T: CanHold.can_hold_16_bits]: Parsley[T] = numberBounded(_16)
    final def decimal16[T: CanHold.can_hold_16_bits]: Parsley[T] = decimalBounded(_16)
    final def hexadecimal16[T: CanHold.can_hold_16_bits]: Parsley[T] = hexadecimalBounded(_16)
    final def octal16[T: CanHold.can_hold_16_bits]: Parsley[T] = octalBounded(_16)
    final def binary16[T: CanHold.can_hold_16_bits]: Parsley[T] = binaryBounded(_16)

    final def number32[T: CanHold.can_hold_32_bits]: Parsley[T] = numberBounded(_32)
    final def decimal32[T: CanHold.can_hold_32_bits]: Parsley[T] = decimalBounded(_32)
    final def hexadecimal32[T: CanHold.can_hold_32_bits]: Parsley[T] = hexadecimalBounded(_32)
    final def octal32[T: CanHold.can_hold_32_bits]: Parsley[T] = octalBounded(_32)
    final def binary32[T: CanHold.can_hold_32_bits]: Parsley[T] = binaryBounded(_32)

    final def number64[T: CanHold.can_hold_64_bits]: Parsley[T] = numberBounded(_64)
    final def decimal64[T: CanHold.can_hold_64_bits]: Parsley[T] = decimalBounded(_64)
    final def hexadecimal64[T: CanHold.can_hold_64_bits]: Parsley[T] = hexadecimalBounded(_64)
    final def octal64[T: CanHold.can_hold_64_bits]: Parsley[T] = octalBounded(_64)
    final def binary64[T: CanHold.can_hold_64_bits]: Parsley[T] = binaryBounded(_64)

    protected [numeric] def bounded[T](number: Parsley[BigInt], bits: Bits, radix: Int)(implicit ev: CanHold[bits.self, T]): Parsley[T]
    protected [numeric] def _decimal: Parsley[BigInt] = decimal
    protected [numeric] def _hexadecimal: Parsley[BigInt] = hexadecimal
    protected [numeric] def _octal: Parsley[BigInt] = octal
    protected [numeric] def _binary: Parsley[BigInt] = binary
    protected [numeric] def _number: Parsley[BigInt] = number

    private def numberBounded[T](bits: Bits)(implicit ev: CanHold[bits.self, T]): Parsley[T] = bounded(_number, bits, 10)
    private def decimalBounded[T](bits: Bits)(implicit ev: CanHold[bits.self, T]): Parsley[T] = bounded(_decimal, bits, 10)
    private def hexadecimalBounded[T](bits: Bits)(implicit ev: CanHold[bits.self, T]): Parsley[T] = bounded(_hexadecimal, bits, 16)
    private def octalBounded[T](bits: Bits)(implicit ev: CanHold[bits.self, T]): Parsley[T] = bounded(_octal, bits, 8)
    private def binaryBounded[T](bits: Bits)(implicit ev: CanHold[bits.self, T]): Parsley[T] = bounded(_binary, bits, 2)
}

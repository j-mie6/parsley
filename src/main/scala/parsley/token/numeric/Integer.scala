/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.numeric

import parsley.Parsley
import parsley.token._

abstract class Integer private[token] {
    def decimal: Parsley[BigInt]
    def hexadecimal: Parsley[BigInt]
    def octal: Parsley[BigInt]
    def binary: Parsley[BigInt]
    def number: Parsley[BigInt]

    def number8[T: CanHold.can_hold_8_bits]: Parsley[T] = numberBounded(_8)
    def decimal8[T: CanHold.can_hold_8_bits]: Parsley[T] = decimalBounded(_8)
    def hexadecimal8[T: CanHold.can_hold_8_bits]: Parsley[T] = hexadecimalBounded(_8)
    def octal8[T: CanHold.can_hold_8_bits]: Parsley[T] = octalBounded(_8)
    def binary8[T: CanHold.can_hold_8_bits]: Parsley[T] = binaryBounded(_8)

    def number16[T: CanHold.can_hold_16_bits]: Parsley[T] = numberBounded(_16)
    def decimal16[T: CanHold.can_hold_16_bits]: Parsley[T] = decimalBounded(_16)
    def hexadecimal16[T: CanHold.can_hold_16_bits]: Parsley[T] = hexadecimalBounded(_16)
    def octal16[T: CanHold.can_hold_16_bits]: Parsley[T] = octalBounded(_16)
    def binary16[T: CanHold.can_hold_16_bits]: Parsley[T] = binaryBounded(_16)

    def number32[T: CanHold.can_hold_32_bits]: Parsley[T] = numberBounded(_32)
    def decimal32[T: CanHold.can_hold_32_bits]: Parsley[T] = decimalBounded(_32)
    def hexadecimal32[T: CanHold.can_hold_32_bits]: Parsley[T] = hexadecimalBounded(_32)
    def octal32[T: CanHold.can_hold_32_bits]: Parsley[T] = octalBounded(_32)
    def binary32[T: CanHold.can_hold_32_bits]: Parsley[T] = binaryBounded(_32)

    def number64[T: CanHold.can_hold_64_bits]: Parsley[T] = numberBounded(_64)
    def decimal64[T: CanHold.can_hold_64_bits]: Parsley[T] = decimalBounded(_64)
    def hexadecimal64[T: CanHold.can_hold_64_bits]: Parsley[T] = hexadecimalBounded(_64)
    def octal64[T: CanHold.can_hold_64_bits]: Parsley[T] = octalBounded(_64)
    def binary64[T: CanHold.can_hold_64_bits]: Parsley[T] = binaryBounded(_64)

    private [numeric] def bounded[T](number: Parsley[BigInt], bits: Bits, radix: Int)(implicit ev: CanHold[bits.self, T]): Parsley[T]
    protected def _decimal: Parsley[BigInt] = decimal
    protected def _hexadecimal: Parsley[BigInt] = hexadecimal
    protected def _octal: Parsley[BigInt] = octal
    protected def _binary: Parsley[BigInt] = binary
    protected def _number: Parsley[BigInt] = number

    private def numberBounded[T](bits: Bits)(implicit ev: CanHold[bits.self, T]): Parsley[T] = bounded(_number, bits, 10)
    private def decimalBounded[T](bits: Bits)(implicit ev: CanHold[bits.self, T]): Parsley[T] = bounded(_decimal, bits, 10)
    private def hexadecimalBounded[T](bits: Bits)(implicit ev: CanHold[bits.self, T]): Parsley[T] = bounded(_hexadecimal, bits, 16)
    private def octalBounded[T](bits: Bits)(implicit ev: CanHold[bits.self, T]): Parsley[T] = bounded(_octal, bits, 8)
    private def binaryBounded[T](bits: Bits)(implicit ev: CanHold[bits.self, T]): Parsley[T] = bounded(_binary, bits, 2)
}

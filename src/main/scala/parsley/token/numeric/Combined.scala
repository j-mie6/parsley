/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.numeric

import parsley.Parsley
import parsley.errors.combinator.{amend, entrench, ErrorMethods}
import parsley.token._

abstract class Combined private[token] {
    def decimal: Parsley[Either[BigInt, BigDecimal]]
    def hexadecimal: Parsley[Either[BigInt, BigDecimal]]
    def octal: Parsley[Either[BigInt, BigDecimal]]
    def binary: Parsley[Either[BigInt, BigDecimal]]
    def number: Parsley[Either[BigInt, BigDecimal]]

    final def number8[T: CanHold.can_hold_8_bits]: Parsley[Either[T, BigDecimal]] = numberBounded(_8)
    final def decimal8[T: CanHold.can_hold_8_bits]: Parsley[Either[T, BigDecimal]] = decimalBounded(_8)
    final def hexadecimal8[T: CanHold.can_hold_8_bits]: Parsley[Either[T, BigDecimal]] = hexadecimalBounded(_8)
    final def octal8[T: CanHold.can_hold_8_bits]: Parsley[Either[T, BigDecimal]] = octalBounded(_8)
    final def binary8[T: CanHold.can_hold_8_bits]: Parsley[Either[T, BigDecimal]] = binaryBounded(_8)

    final def number8Float[T: CanHold.can_hold_8_bits]: Parsley[Either[T, Float]] = ensureFloat(numberBounded(_8))
    final def decimal8Float[T: CanHold.can_hold_8_bits]: Parsley[Either[T, Float]] = ensureFloat(decimalBounded(_8))
    final def hexadecimal8Float[T: CanHold.can_hold_8_bits]: Parsley[Either[T, Float]] = ensureFloat(hexadecimalBounded(_8))
    final def octal8Float[T: CanHold.can_hold_8_bits]: Parsley[Either[T, Float]] = ensureFloat(octalBounded(_8))
    final def binary8Float[T: CanHold.can_hold_8_bits]: Parsley[Either[T, Float]] = ensureFloat(binaryBounded(_8))

    final def number8Double[T: CanHold.can_hold_8_bits]: Parsley[Either[T, Double]] = ensureDouble(numberBounded(_8))
    final def decimal8Double[T: CanHold.can_hold_8_bits]: Parsley[Either[T, Double]] = ensureDouble(decimalBounded(_8))
    final def hexadecimal8Double[T: CanHold.can_hold_8_bits]: Parsley[Either[T, Double]] = ensureDouble(hexadecimalBounded(_8))
    final def octal8Double[T: CanHold.can_hold_8_bits]: Parsley[Either[T, Double]] = ensureDouble(octalBounded(_8))
    final def binary8Double[T: CanHold.can_hold_8_bits]: Parsley[Either[T, Double]] = ensureDouble(binaryBounded(_8))

    final def number16[T: CanHold.can_hold_16_bits]: Parsley[Either[T, BigDecimal]] = numberBounded(_16)
    final def decimal16[T: CanHold.can_hold_16_bits]: Parsley[Either[T, BigDecimal]] = decimalBounded(_16)
    final def hexadecimal16[T: CanHold.can_hold_16_bits]: Parsley[Either[T, BigDecimal]] = hexadecimalBounded(_16)
    final def octal16[T: CanHold.can_hold_16_bits]: Parsley[Either[T, BigDecimal]] = octalBounded(_16)
    final def binary16[T: CanHold.can_hold_16_bits]: Parsley[Either[T, BigDecimal]] = binaryBounded(_16)

    final def number16Float[T: CanHold.can_hold_16_bits]: Parsley[Either[T, Float]] = ensureFloat(numberBounded(_16))
    final def decimal16Float[T: CanHold.can_hold_16_bits]: Parsley[Either[T, Float]] = ensureFloat(decimalBounded(_16))
    final def hexadecimal16Float[T: CanHold.can_hold_16_bits]: Parsley[Either[T, Float]] = ensureFloat(hexadecimalBounded(_16))
    final def octal16Float[T: CanHold.can_hold_16_bits]: Parsley[Either[T, Float]] = ensureFloat(octalBounded(_16))
    final def binary16Float[T: CanHold.can_hold_16_bits]: Parsley[Either[T, Float]] = ensureFloat(binaryBounded(_16))

    final def number16Double[T: CanHold.can_hold_16_bits]: Parsley[Either[T, Double]] = ensureDouble(numberBounded(_16))
    final def decimal16Double[T: CanHold.can_hold_16_bits]: Parsley[Either[T, Double]] = ensureDouble(decimalBounded(_16))
    final def hexadecimal16Double[T: CanHold.can_hold_16_bits]: Parsley[Either[T, Double]] = ensureDouble(hexadecimalBounded(_16))
    final def octal16Double[T: CanHold.can_hold_16_bits]: Parsley[Either[T, Double]] = ensureDouble(octalBounded(_16))
    final def binary16Double[T: CanHold.can_hold_16_bits]: Parsley[Either[T, Double]] = ensureDouble(binaryBounded(_16))

    final def number32[T: CanHold.can_hold_32_bits]: Parsley[Either[T, BigDecimal]] = numberBounded(_32)
    final def decimal32[T: CanHold.can_hold_32_bits]: Parsley[Either[T, BigDecimal]] = decimalBounded(_32)
    final def hexadecimal32[T: CanHold.can_hold_32_bits]: Parsley[Either[T, BigDecimal]] = hexadecimalBounded(_32)
    final def octal32[T: CanHold.can_hold_32_bits]: Parsley[Either[T, BigDecimal]] = octalBounded(_32)
    final def binary32[T: CanHold.can_hold_32_bits]: Parsley[Either[T, BigDecimal]] = binaryBounded(_32)

    final def number32Float[T: CanHold.can_hold_32_bits]: Parsley[Either[T, Float]] = ensureFloat(numberBounded(_32))
    final def decimal32Float[T: CanHold.can_hold_32_bits]: Parsley[Either[T, Float]] = ensureFloat(decimalBounded(_32))
    final def hexadecimal32Float[T: CanHold.can_hold_32_bits]: Parsley[Either[T, Float]] = ensureFloat(hexadecimalBounded(_32))
    final def octal32Float[T: CanHold.can_hold_32_bits]: Parsley[Either[T, Float]] = ensureFloat(octalBounded(_32))
    final def binary32Float[T: CanHold.can_hold_32_bits]: Parsley[Either[T, Float]] = ensureFloat(binaryBounded(_32))

    final def number32Double[T: CanHold.can_hold_32_bits]: Parsley[Either[T, Double]] = ensureDouble(numberBounded(_32))
    final def decimal32Double[T: CanHold.can_hold_32_bits]: Parsley[Either[T, Double]] = ensureDouble(decimalBounded(_32))
    final def hexadecimal32Double[T: CanHold.can_hold_32_bits]: Parsley[Either[T, Double]] = ensureDouble(hexadecimalBounded(_32))
    final def octal32Double[T: CanHold.can_hold_32_bits]: Parsley[Either[T, Double]] = ensureDouble(octalBounded(_32))
    final def binary32Double[T: CanHold.can_hold_32_bits]: Parsley[Either[T, Double]] = ensureDouble(binaryBounded(_32))

    final def number64[T: CanHold.can_hold_64_bits]: Parsley[Either[T, BigDecimal]] = numberBounded(_64)
    final def decimal64[T: CanHold.can_hold_64_bits]: Parsley[Either[T, BigDecimal]] = decimalBounded(_64)
    final def hexadecimal64[T: CanHold.can_hold_64_bits]: Parsley[Either[T, BigDecimal]] = hexadecimalBounded(_64)
    final def octal64[T: CanHold.can_hold_64_bits]: Parsley[Either[T, BigDecimal]] = octalBounded(_64)
    final def binary64[T: CanHold.can_hold_64_bits]: Parsley[Either[T, BigDecimal]] = binaryBounded(_64)

    final def number64Float[T: CanHold.can_hold_64_bits]: Parsley[Either[T, Float]] = ensureFloat(numberBounded(_64))
    final def decimal64Float[T: CanHold.can_hold_64_bits]: Parsley[Either[T, Float]] = ensureFloat(decimalBounded(_64))
    final def hexadecimal64Float[T: CanHold.can_hold_64_bits]: Parsley[Either[T, Float]] = ensureFloat(hexadecimalBounded(_64))
    final def octal64Float[T: CanHold.can_hold_64_bits]: Parsley[Either[T, Float]] = ensureFloat(octalBounded(_64))
    final def binary64Float[T: CanHold.can_hold_64_bits]: Parsley[Either[T, Float]] = ensureFloat(binaryBounded(_64))

    final def number64Double[T: CanHold.can_hold_64_bits]: Parsley[Either[T, Double]] = ensureDouble(numberBounded(_64))
    final def decimal64Double[T: CanHold.can_hold_64_bits]: Parsley[Either[T, Double]] = ensureDouble(decimalBounded(_64))
    final def hexadecimal64Double[T: CanHold.can_hold_64_bits]: Parsley[Either[T, Double]] = ensureDouble(hexadecimalBounded(_64))
    final def octal64Double[T: CanHold.can_hold_64_bits]: Parsley[Either[T, Double]] = ensureDouble(octalBounded(_64))
    final def binary64Double[T: CanHold.can_hold_64_bits]: Parsley[Either[T, Double]] = ensureDouble(binaryBounded(_64))

    // TODO: the bounded should be combined with the ensuring, for better performance
    protected [numeric] def bounded[T](number: Parsley[Either[BigInt, BigDecimal]], bits: Bits, radix: Int)
                                      (implicit ev: CanHold[bits.self, T]): Parsley[Either[T, BigDecimal]]
    protected [numeric] def _decimal: Parsley[Either[BigInt, BigDecimal]] = decimal
    protected [numeric] def _hexadecimal: Parsley[Either[BigInt, BigDecimal]] = hexadecimal
    protected [numeric] def _octal: Parsley[Either[BigInt, BigDecimal]] = octal
    protected [numeric] def _binary: Parsley[Either[BigInt, BigDecimal]] = binary
    protected [numeric] def _number: Parsley[Either[BigInt, BigDecimal]] = number

    private def numberBounded[T](bits: Bits)(implicit ev: CanHold[bits.self, T]): Parsley[Either[T, BigDecimal]] = bounded(_number, bits, 10)
    private def decimalBounded[T](bits: Bits)(implicit ev: CanHold[bits.self, T]): Parsley[Either[T, BigDecimal]] = bounded(_decimal, bits, 10)
    private def hexadecimalBounded[T](bits: Bits)(implicit ev: CanHold[bits.self, T]): Parsley[Either[T, BigDecimal]] = bounded(_hexadecimal, bits, 16)
    private def octalBounded[T](bits: Bits)(implicit ev: CanHold[bits.self, T]): Parsley[Either[T, BigDecimal]] = bounded(_octal, bits, 8)
    private def binaryBounded[T](bits: Bits)(implicit ev: CanHold[bits.self, T]): Parsley[Either[T, BigDecimal]] = bounded(_binary, bits, 2)

    protected [numeric] def ensureFloat[T](number: Parsley[Either[T, BigDecimal]]): Parsley[Either[T, Float]] = amend {
        entrench(number).collectMsg(n => Seq(s"$n cannot be represented exactly as a IEEE 754 single-precision float")) {
            case Left(n) => Left(n)
            case Right(n) if n.isBinaryFloat || n.isDecimalFloat || n.isExactFloat => Right(n.toFloat)
        }
    }

    protected [numeric] def ensureDouble[T](number: Parsley[Either[T, BigDecimal]]): Parsley[Either[T, Double]] = amend {
        entrench(number).collectMsg(n => Seq(s"$n cannot be represented exactly as a IEEE 754 double-precision float")) {
            case Left(n) => Left(n)
            case Right(n) if n.isBinaryDouble || n.isDecimalDouble || n.isExactDouble => Right(n.toDouble)
        }
    }
}

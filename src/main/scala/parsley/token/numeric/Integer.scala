/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.numeric

import parsley.Parsley, Parsley.pure
import parsley.character.{digit, hexDigit, octDigit, oneOf}
import parsley.combinator.optional
import parsley.extension.OperatorSugar
import parsley.implicits.character.charLift
import parsley.token.descriptions.numeric.NumericDesc

/** This class defines a uniform interface for defining parsers for integer
  * literals, independent of how whitespace should be handled after the literal
  * or whether the literal should allow for negative numbers.
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
abstract class Integer private[token] (private [numeric] val desc: NumericDesc) {
    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    def decimal: Parsley[BigInt]
    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    def hexadecimal: Parsley[BigInt]
    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    def octal: Parsley[BigInt]
    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    def binary: Parsley[BigInt]
    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    def number: Parsley[BigInt]

    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    final def number8[T: CanHold.can_hold_8_bits]: Parsley[T] = numberBounded(_8)
    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    final def decimal8[T: CanHold.can_hold_8_bits]: Parsley[T] = decimalBounded(_8)
    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    final def hexadecimal8[T: CanHold.can_hold_8_bits]: Parsley[T] = hexadecimalBounded(_8)
    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    final def octal8[T: CanHold.can_hold_8_bits]: Parsley[T] = octalBounded(_8)
    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    final def binary8[T: CanHold.can_hold_8_bits]: Parsley[T] = binaryBounded(_8)

    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    final def number16[T: CanHold.can_hold_16_bits]: Parsley[T] = numberBounded(_16)
    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    final def decimal16[T: CanHold.can_hold_16_bits]: Parsley[T] = decimalBounded(_16)
    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    final def hexadecimal16[T: CanHold.can_hold_16_bits]: Parsley[T] = hexadecimalBounded(_16)
    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    final def octal16[T: CanHold.can_hold_16_bits]: Parsley[T] = octalBounded(_16)
    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    final def binary16[T: CanHold.can_hold_16_bits]: Parsley[T] = binaryBounded(_16)

    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    final def number32[T: CanHold.can_hold_32_bits]: Parsley[T] = numberBounded(_32)
    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    final def decimal32[T: CanHold.can_hold_32_bits]: Parsley[T] = decimalBounded(_32)
    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    final def hexadecimal32[T: CanHold.can_hold_32_bits]: Parsley[T] = hexadecimalBounded(_32)
    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    final def octal32[T: CanHold.can_hold_32_bits]: Parsley[T] = octalBounded(_32)
    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    final def binary32[T: CanHold.can_hold_32_bits]: Parsley[T] = binaryBounded(_32)

    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    final def number64[T: CanHold.can_hold_64_bits]: Parsley[T] = numberBounded(_64)
    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    final def decimal64[T: CanHold.can_hold_64_bits]: Parsley[T] = decimalBounded(_64)
    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    final def hexadecimal64[T: CanHold.can_hold_64_bits]: Parsley[T] = hexadecimalBounded(_64)
    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
    final def octal64[T: CanHold.can_hold_64_bits]: Parsley[T] = octalBounded(_64)
    /** TODO:
      *
      * @since 4.0.0
      * @note $disclaimer
      */
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

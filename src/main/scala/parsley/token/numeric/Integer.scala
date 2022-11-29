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
  *
  * @define bounded1
  *   This parser will behave the same as
  *
  * @define bounded2
  *   except it will ensure that the resulting `BigInt` is a valid
  *
  * @define bounded3
  *   number. The resulting number will be converted to the given
  *   type `T`, which must be able to losslessly store the parsed
  *   value; this is enforced by the constraint on the type. This
  *   accounts for unsignedness when necessary.
  *
  * @define bounded4 the desired type of the result, defaulting to
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

    // $COVERAGE-OFF$
    // It's not so important these are tested, they are just wrappers around the bottom ones
    /** $bounded1 [[number `number`]] $bounded2 8-bit $bounded3
      *
      * @tparam T $bounded4 `Byte`
      * @since 4.0.0
      * @note $disclaimer
      */
    @inline final def number8[T: CanHold.can_hold_8_bits]: Parsley[T] = numberBounded(_8)
    /** $bounded1 [[decimal `decimal`]] $bounded2 8-bit $bounded3
      *
      * @tparam T $bounded4 `Byte`
      * @since 4.0.0
      * @note $disclaimer
      */
    @inline final def decimal8[T: CanHold.can_hold_8_bits]: Parsley[T] = decimalBounded(_8)
    /** $bounded1 [[hexadecimal `hexadecimal`]] $bounded2 8-bit $bounded3
      *
      * @tparam T $bounded4 `Byte`
      * @since 4.0.0
      * @note $disclaimer
      */
    @inline final def hexadecimal8[T: CanHold.can_hold_8_bits]: Parsley[T] = hexadecimalBounded(_8)
    /** $bounded1 [[octal `octal`]] $bounded2 8-bit $bounded3
      *
      * @tparam T $bounded4 `Byte`
      * @since 4.0.0
      * @note $disclaimer
      */
    @inline final def octal8[T: CanHold.can_hold_8_bits]: Parsley[T] = octalBounded(_8)
    /** $bounded1 [[binary `binary`]] $bounded2 8-bit $bounded3
      *
      * @tparam T $bounded4 `Byte`
      * @since 4.0.0
      * @note $disclaimer
      */
    @inline final def binary8[T: CanHold.can_hold_8_bits]: Parsley[T] = binaryBounded(_8)

    /** $bounded1 [[number `number`]] $bounded2 16-bit $bounded3
      *
      * @tparam T $bounded4 `Short`
      * @since 4.0.0
      * @note $disclaimer
      */
    @inline final def number16[T: CanHold.can_hold_16_bits]: Parsley[T] = numberBounded(_16)
    /** $bounded1 [[decimal `decimal`]] $bounded2 16-bit $bounded3
      *
      * @tparam T $bounded4 `Short`
      * @since 4.0.0
      * @note $disclaimer
      */
    @inline final def decimal16[T: CanHold.can_hold_16_bits]: Parsley[T] = decimalBounded(_16)
    /** $bounded1 [[hexadecimal `hexadecimal`]] $bounded2 16-bit $bounded3
      *
      * @tparam T $bounded4 `Short`
      * @since 4.0.0
      * @note $disclaimer
      */
    @inline final def hexadecimal16[T: CanHold.can_hold_16_bits]: Parsley[T] = hexadecimalBounded(_16)
    /** $bounded1 [[octal `octal`]] $bounded2 16-bit $bounded3
      *
      * @tparam T $bounded4 `Short`
      * @since 4.0.0
      * @note $disclaimer
      */
    @inline final def octal16[T: CanHold.can_hold_16_bits]: Parsley[T] = octalBounded(_16)
    /** $bounded1 [[binary `binary`]] $bounded2 16-bit $bounded3
      *
      * @tparam T $bounded4 `Short`
      * @since 4.0.0
      * @note $disclaimer
      */
    @inline final def binary16[T: CanHold.can_hold_16_bits]: Parsley[T] = binaryBounded(_16)

    /** $bounded1 [[number `number`]] $bounded2 32-bit $bounded3
      *
      * @tparam T $bounded4 `Int`
      * @since 4.0.0
      * @note $disclaimer
      */
    @inline final def number32[T: CanHold.can_hold_32_bits]: Parsley[T] = numberBounded(_32)
    /** $bounded1 [[decimal `decimal`]] $bounded2 32-bit $bounded3
      *
      * @tparam T $bounded4 `Int`
      * @since 4.0.0
      * @note $disclaimer
      */
    @inline final def decimal32[T: CanHold.can_hold_32_bits]: Parsley[T] = decimalBounded(_32)
    /** $bounded1 [[hexadecimal `hexadecimal`]] $bounded2 32-bit $bounded3
      *
      * @tparam T $bounded4 `Int`
      * @since 4.0.0
      * @note $disclaimer
      */
    @inline final def hexadecimal32[T: CanHold.can_hold_32_bits]: Parsley[T] = hexadecimalBounded(_32)
    /** $bounded1 [[octal `octal`]] $bounded2 32-bit $bounded3
      *
      * @tparam T $bounded4 `Int`
      * @since 4.0.0
      * @note $disclaimer
      */
    @inline final def octal32[T: CanHold.can_hold_32_bits]: Parsley[T] = octalBounded(_32)
    /** $bounded1 [[binary `binary`]] $bounded2 32-bit $bounded3
      *
      * @tparam T $bounded4 `Int`
      * @since 4.0.0
      * @note $disclaimer
      */
    @inline final def binary32[T: CanHold.can_hold_32_bits]: Parsley[T] = binaryBounded(_32)

    /** $bounded1 [[number `number`]] $bounded2 64-bit $bounded3
      *
      * @tparam T $bounded4 `Long`
      * @since 4.0.0
      * @note $disclaimer
      */
    @inline final def number64[T: CanHold.can_hold_64_bits]: Parsley[T] = numberBounded(_64)
    /** $bounded1 [[decimal `decimal`]] $bounded2 64-bit $bounded3
      *
      * @tparam T $bounded4 `Long`
      * @since 4.0.0
      * @note $disclaimer
      */
    @inline final def decimal64[T: CanHold.can_hold_64_bits]: Parsley[T] = decimalBounded(_64)
    /** $bounded1 [[hexadecimal `hexadecimal`]] $bounded2 64-bit $bounded3
      *
      * @tparam T $bounded4 `Long`
      * @since 4.0.0
      * @note $disclaimer
      */
    @inline final def hexadecimal64[T: CanHold.can_hold_64_bits]: Parsley[T] = hexadecimalBounded(_64)
    /** $bounded1 [[octal `octal`]] $bounded2 64-bit $bounded3
      *
      * @tparam T $bounded4 `Long`
      * @since 4.0.0
      * @note $disclaimer
      */
    @inline final def octal64[T: CanHold.can_hold_64_bits]: Parsley[T] = octalBounded(_64)
    /** $bounded1 [[binary `binary`]] $bounded2 64-bit $bounded3
      *
      * @tparam T $bounded4 `Long`
      * @since 4.0.0
      * @note $disclaimer
      */
    @inline final def binary64[T: CanHold.can_hold_64_bits]: Parsley[T] = binaryBounded(_64)


    protected [numeric] def bounded[T](number: Parsley[BigInt], bits: Bits, radix: Int)(implicit ev: CanHold[bits.self, T]): Parsley[T]
    protected [numeric] def _decimal: Parsley[BigInt] = decimal
    protected [numeric] def _hexadecimal: Parsley[BigInt] = hexadecimal
    protected [numeric] def _octal: Parsley[BigInt] = octal
    protected [numeric] def _binary: Parsley[BigInt] = binary
    protected [numeric] def _number: Parsley[BigInt] = number
    // $COVERAGE-ON$

    private def numberBounded[T](bits: Bits)(implicit ev: CanHold[bits.self, T]): Parsley[T] = bounded(_number, bits, 10)
    private def decimalBounded[T](bits: Bits)(implicit ev: CanHold[bits.self, T]): Parsley[T] = bounded(_decimal, bits, 10)
    private def hexadecimalBounded[T](bits: Bits)(implicit ev: CanHold[bits.self, T]): Parsley[T] = bounded(_hexadecimal, bits, 16)
    private def octalBounded[T](bits: Bits)(implicit ev: CanHold[bits.self, T]): Parsley[T] = bounded(_octal, bits, 8)
    private def binaryBounded[T](bits: Bits)(implicit ev: CanHold[bits.self, T]): Parsley[T] = bounded(_binary, bits, 2)
}

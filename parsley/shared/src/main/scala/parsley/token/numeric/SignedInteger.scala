/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.numeric

import parsley.Parsley, Parsley.attempt
import parsley.token.descriptions.numeric.NumericDesc
import parsley.token.errors.{ErrorConfig, LabelWithExplainConfig}

import parsley.internal.deepembedding.Sign.IntType
import parsley.internal.deepembedding.singletons

private [token] final class SignedInteger(desc: NumericDesc, unsigned: UnsignedInteger, err: ErrorConfig) extends Integer(desc) {
    private val sign = new Parsley(new singletons.Sign[IntType.resultType](IntType, desc.positiveSign))

    override lazy val _decimal: Parsley[BigInt] = attempt(sign <*> err.labelIntegerDecimalEnd(unsigned._decimal))
    override lazy val _hexadecimal: Parsley[BigInt] = attempt(sign <*> err.labelIntegerHexadecimalEnd(unsigned._hexadecimal))
    override lazy val _octal: Parsley[BigInt] = attempt(sign <*> err.labelIntegerOctalEnd(unsigned._octal))
    override lazy val _binary: Parsley[BigInt] = attempt(sign <*> err.labelIntegerBinaryEnd(unsigned._binary))
    override lazy val _number: Parsley[BigInt] = attempt(sign <*> err.labelIntegerNumberEnd(unsigned._number))

    override def decimal: Parsley[BigInt] = err.labelIntegerSignedDecimal.apply(_decimal)
    override def hexadecimal: Parsley[BigInt] = err.labelIntegerSignedHexadecimal.apply(_hexadecimal)
    override def octal: Parsley[BigInt] = err.labelIntegerSignedOctal.apply(_octal)
    override def binary: Parsley[BigInt] = err.labelIntegerSignedBinary.apply(_binary)
    override def number: Parsley[BigInt] = err.labelIntegerSignedNumber.apply(_number)

    override protected [numeric] def bounded[T](number: Parsley[BigInt], bits: Bits, radix: Int, label: (ErrorConfig, Boolean) => LabelWithExplainConfig)
                                               (implicit ev: CanHold[bits.self,T]): Parsley[T] = label(err, false) {
        err.messageIntOutOfBounds(bits.lowerSigned, bits.upperSigned, radix).collect(number) {
            case x if bits.lowerSigned <= x && x <= bits.upperSigned => ev.fromBigInt(x)
        }
    }
}

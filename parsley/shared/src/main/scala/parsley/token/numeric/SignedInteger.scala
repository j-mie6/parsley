/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.numeric

import parsley.Parsley, Parsley.attempt
import parsley.errors.combinator.ErrorMethods
import parsley.token.descriptions.numeric.NumericDesc
import parsley.token.errors.ErrorConfig

import parsley.internal.deepembedding.Sign.IntType
import parsley.internal.deepembedding.singletons

private [token] final class SignedInteger(desc: NumericDesc, unsigned: UnsignedInteger, err: ErrorConfig) extends Integer(desc) {
    private val sign = new Parsley(new singletons.Sign[IntType.resultType](IntType, desc.positiveSign))

    override lazy val _decimal: Parsley[BigInt] = attempt(sign <*> ErrorConfig.label(err.labelIntegerDecimalEnd)(unsigned._decimal))
    override lazy val _hexadecimal: Parsley[BigInt] = attempt(sign <*> ErrorConfig.label(err.labelIntegerHexadecimalEnd)(unsigned._hexadecimal))
    override lazy val _octal: Parsley[BigInt] = attempt(sign <*> ErrorConfig.label(err.labelIntegerOctalEnd)(unsigned._octal))
    override lazy val _binary: Parsley[BigInt] = attempt(sign <*> ErrorConfig.label(err.labelIntegerBinaryEnd)(unsigned._binary))
    override lazy val _number: Parsley[BigInt] = attempt(sign <*> ErrorConfig.label(err.labelIntegerNumberEnd)(unsigned._number))

    override def decimal: Parsley[BigInt] = ErrorConfig.label(err.labelIntegerSignedDecimal)(_decimal)
    override def hexadecimal: Parsley[BigInt] = ErrorConfig.label(err.labelIntegerSignedHexadecimal)(_hexadecimal)
    override def octal: Parsley[BigInt] = ErrorConfig.label(err.labelIntegerSignedOctal)(_octal)
    override def binary: Parsley[BigInt] = ErrorConfig.label(err.labelIntegerSignedBinary)(_binary)
    override def number: Parsley[BigInt] = ErrorConfig.label(err.labelIntegerSignedNumber)(_number)
    
    override protected [numeric] def bounded[T](number: Parsley[BigInt], bits: Bits, radix: Int, label: (ErrorConfig, Boolean) => Option[String])
                                               (implicit ev: CanHold[bits.self,T]): Parsley[T] = ErrorConfig.label(label(err, false)) {
        number.collectMsg(x => if (x > bits.upperSigned) err.messageIntTooLarge(x, bits.upperSigned, radix)
                               else                      err.messageIntTooSmall(x, bits.lowerSigned, radix)) {
            case x if bits.lowerSigned <= x && x <= bits.upperSigned => ev.fromBigInt(x)
        }
    }
}

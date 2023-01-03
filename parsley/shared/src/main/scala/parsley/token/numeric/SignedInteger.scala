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
    override lazy val decimal: Parsley[BigInt] = ErrorConfig.label(err.labelIntegerSignedDecimal)(attempt(sign <*> unsigned.decimalNoLabel))
    override lazy val hexadecimal: Parsley[BigInt] = ErrorConfig.label(err.labelIntegerSignedHexadecimal)(attempt(sign <*> unsigned.hexadecimalNoLabel))
    override lazy val octal: Parsley[BigInt] = ErrorConfig.label(err.labelIntegerSignedOctal)(attempt(sign <*> unsigned.octalNoLabel))
    override lazy val binary: Parsley[BigInt] = ErrorConfig.label(err.labelIntegerSignedBinary)(attempt(sign <*> unsigned.binaryNoLabel))
    override lazy val number: Parsley[BigInt] = ErrorConfig.label(err.labelIntegerSignedNumber)(attempt(sign <*> unsigned.numberNoLabel))
    override protected [numeric] def bounded[T](number: Parsley[BigInt], bits: Bits, radix: Int, label: (ErrorConfig, Boolean) => Option[String])
                                               (implicit ev: CanHold[bits.self,T]): Parsley[T] = ErrorConfig.label(label(err, false)) {
        number.collectMsg(x => if (x > bits.upperSigned) err.messageIntTooLarge(x, bits.upperSigned, radix)
                               else                      err.messageIntTooSmall(x, bits.lowerSigned, radix)) {
            case x if bits.lowerSigned <= x && x <= bits.upperSigned => ev.fromBigInt(x)
        }
    }
}

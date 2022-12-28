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

private [token] final class SignedInteger(desc: NumericDesc, unsigned: Integer, err: ErrorConfig) extends Integer(desc) {
    private val sign = new Parsley(new singletons.Sign[IntType.resultType](IntType, desc.positiveSign))
    override lazy val decimal: Parsley[BigInt] = attempt(sign <*> unsigned.decimal)
    override lazy val hexadecimal: Parsley[BigInt] = attempt(sign <*> unsigned.hexadecimal)
    override lazy val octal: Parsley[BigInt] = attempt(sign <*> unsigned.octal)
    override lazy val binary: Parsley[BigInt] = attempt(sign <*> unsigned.binary)
    override lazy val number: Parsley[BigInt] = attempt(sign <*> unsigned.number)
    override protected [numeric] def bounded[T](number: Parsley[BigInt], bits: Bits, radix: Int)(implicit ev: CanHold[bits.self,T]): Parsley[T] = {
        number.collectMsg(x => if (x > bits.upperSigned) err.messageIntTooLarge(x, bits.upperSigned, radix)
                               else                      err.messageIntTooSmall(x, bits.lowerSigned, radix)) {
            case x if bits.lowerSigned <= x && x <= bits.upperSigned => ev.fromBigInt(x)
        }
    }
}

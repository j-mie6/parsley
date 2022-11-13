/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.numeric

import parsley.Parsley, Parsley.attempt
import parsley.errors.combinator.{amend, entrench, ErrorMethods}
import parsley.token.descriptions.numeric.NumericDesc

import parsley.internal.deepembedding.Sign.CombinedType
import parsley.internal.deepembedding.singletons

private [token] final class SignedCombined(desc: NumericDesc, unsigned: Combined) extends Combined {
    private val sign = new Parsley(new singletons.Sign[CombinedType.resultType](CombinedType, desc.positiveSign))

    override def decimal: Parsley[Either[BigInt,BigDecimal]] = attempt(sign <*> unsigned.decimal)
    override def hexadecimal: Parsley[Either[BigInt,BigDecimal]] = attempt(sign <*> unsigned.hexadecimal)
    override def octal: Parsley[Either[BigInt,BigDecimal]] = attempt(sign <*> unsigned.octal)
    override def binary: Parsley[Either[BigInt,BigDecimal]] = attempt(sign <*> unsigned.binary)
    override def number: Parsley[Either[BigInt,BigDecimal]] = attempt(sign <*> unsigned.number)

    override protected[numeric] def bounded[T](number: Parsley[Either[BigInt,BigDecimal]], bits: Bits, radix: Int)
                                              (implicit ev: CanHold[bits.self,T]): Parsley[Either[T,BigDecimal]] = amend {
        entrench(number).collectMsg(ex => {
            val Left(x) = ex
            Seq(if (x > bits.upperSigned) s"literal $x is larger than the max value of ${bits.upperSigned}"
                else                      s"literal $x is less than the min value of ${bits.lowerSigned}")
        }) {
            case Left(x) if bits.lowerSigned <= x && x <= bits.upperSigned => Left(ev.fromBigInt(x))
            case Right(y) => Right(y)
        }
    }
}

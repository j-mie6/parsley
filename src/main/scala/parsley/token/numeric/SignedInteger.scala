/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.numeric

import parsley.Parsley, Parsley.attempt
import parsley.errors.combinator.{amend, entrench, ErrorMethods}
import parsley.token.{Bits, CanHold}

import parsley.internal.deepembedding.singletons
import parsley.internal.deepembedding.Sign.IntType

private [token] final class SignedInteger(unsigned: Integer) extends Integer {
    private val sign = new Parsley(new singletons.Sign[IntType.resultType](IntType))
    override lazy val decimal: Parsley[BigInt] = attempt(sign <*> unsigned.decimal)
    override lazy val hexadecimal: Parsley[BigInt] = attempt(sign <*> unsigned.hexadecimal)
    override lazy val octal: Parsley[BigInt] = attempt(sign <*> unsigned.octal)
    override lazy val binary: Parsley[BigInt] = attempt(sign <*> unsigned.binary)
    override lazy val number: Parsley[BigInt] = attempt(sign <*> unsigned.number)

    // TODO: render in the "native" radix
    override protected [numeric] def bounded[T](number: Parsley[BigInt], bits: Bits, radix: Int)(implicit ev: CanHold[bits.self,T]): Parsley[T] = amend {
        entrench(number).collectMsg(x => Seq(if (x > bits.upperSigned) s"literal $x is larger than the max value of ${bits.upperSigned}"
                                             else                      s"literal $x is less than the min value of ${bits.lowerSigned}")) {
            case x if bits.lowerSigned <= x && x <= bits.upperSigned => ev.fromBigInt(x)
        }
    }
}

/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.numeric

import parsley.Parsley, Parsley.{attempt, pure}
import parsley.character.{digit, hexDigit, octDigit, oneOf}
import parsley.combinator.choice
import parsley.errors.combinator.{amend, entrench, ErrorMethods}
import parsley.implicits.character.charLift
import parsley.token.{Bits, CanHold}
import parsley.token.descriptions.NumericDesc

private [token] final class UnsignedInteger(desc: NumericDesc) extends Integer {
    override lazy val decimal: Parsley[BigInt] = plainDecimal
    override lazy val hexadecimal: Parsley[BigInt] = attempt('0' *> noZeroHexadecimal)
    override lazy val octal: Parsley[BigInt] = attempt('0' *> noZeroOctal)
    override lazy val binary: Parsley[BigInt] = attempt('0' *> noZeroBinary)
    override lazy val number: Parsley[BigInt] = {
        val zeroLead = '0' *> (noZeroHexadecimal <|> noZeroOctal <|> noZeroBinary <|> decimal <|> pure(BigInt(0)))
        attempt(zeroLead <|> decimal)
    }

    // TODO: Using choice here will generate a jump table, which will be nicer for `number` (this requires enhancements to the jumptable optimisation)
    // TODO: Leave these as defs so they get inlined into number for the jumptable optimisation
    private val noZeroHexadecimal = oneOf('x', 'X') *> plainHexadecimal
    private val noZeroOctal = oneOf('o', 'O') *> plainOctal
    private val noZeroBinary = oneOf('b', 'B') *> plainBinary

    // TODO: render in the "native" radix
    override protected [numeric] def bounded[T](number: Parsley[BigInt], bits: Bits, radix: Int)(implicit ev: CanHold[bits.self,T]): Parsley[T] = amend {
        entrench(number).collectMsg(x => Seq(s"literal $x is larger than the max value of ${bits.upperUnsigned}")) {
            case x if x <= bits.upperUnsigned => ev.fromBigInt(x)
        }
    }
}

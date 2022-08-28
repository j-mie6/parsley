/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.numeric

import parsley.Parsley, Parsley.attempt
import parsley.character.{digit, hexDigit, octDigit, oneOf}
import parsley.errors.combinator.{amend, entrench, ErrorMethods}
import parsley.implicits.character.charLift
import parsley.token.{Bits, CanHold}
import parsley.token.descriptions.NumericDesc

private [token] final class UnsignedInteger(desc: NumericDesc) extends Integer {
    override lazy val decimal: Parsley[BigInt] = ofRadix(10, digit)
    override lazy val hexadecimal: Parsley[BigInt] = attempt('0' *> oneOf('x', 'X') *> ofRadix(16, hexDigit))
    override lazy val octal: Parsley[BigInt] = attempt('0' *> oneOf('o', 'O') *> ofRadix(8, octDigit))
    override lazy val binary: Parsley[BigInt] = attempt('0' *> oneOf('b', 'B') *> ofRadix(2, oneOf('0', '1')))
    override lazy val number: Parsley[BigInt] = hexadecimal <|> octal <|> binary <|> decimal

    // TODO: render in the "native" radix
    override protected [numeric] def bounded[T](number: Parsley[BigInt], bits: Bits, radix: Int)(implicit ev: CanHold[bits.self,T]): Parsley[T] = amend {
        entrench(number).collectMsg(x => Seq(s"literal $x is larger than the max value of ${bits.upperUnsigned}")) {
            case x if x <= bits.upperUnsigned => ev.fromBigInt(x)
        }
    }

    private def ofRadix(radix: Int, digit: Parsley[Char]) = digit.foldLeft1[BigInt](0)((x, d) => x*radix + d.asDigit)
}

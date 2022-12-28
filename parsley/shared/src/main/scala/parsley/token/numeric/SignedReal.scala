/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.numeric

import parsley.Parsley, Parsley.attempt
import parsley.token.descriptions.numeric.NumericDesc
import parsley.token.errors.ErrorConfig

import parsley.internal.deepembedding.Sign.DoubleType
import parsley.internal.deepembedding.singletons

private [token] final class SignedReal(desc: NumericDesc, unsigned: Real, err: ErrorConfig) extends Real(err) {
    private val sign = new Parsley(new singletons.Sign[DoubleType.resultType](DoubleType, desc.positiveSign))

    override lazy val decimal: Parsley[BigDecimal] = attempt(sign <*> unsigned.decimal)
    override lazy val hexadecimal: Parsley[BigDecimal] = attempt(sign <*> unsigned.hexadecimal)
    override lazy val octal: Parsley[BigDecimal] = attempt(sign <*> unsigned.octal)
    override lazy val binary: Parsley[BigDecimal] = attempt(sign <*> unsigned.binary)
    override lazy val number: Parsley[BigDecimal] = attempt(sign <*> unsigned.number)
}

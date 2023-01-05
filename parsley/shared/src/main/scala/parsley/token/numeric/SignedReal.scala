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

    override lazy val _decimal: Parsley[BigDecimal] = attempt(sign <*> unsigned._decimal)
    override lazy val _hexadecimal: Parsley[BigDecimal] = attempt(sign <*> unsigned._hexadecimal)
    override lazy val _octal: Parsley[BigDecimal] = attempt(sign <*> unsigned._octal)
    override lazy val _binary: Parsley[BigDecimal] = attempt(sign <*> unsigned._binary)
    override lazy val _number: Parsley[BigDecimal] = attempt(sign <*> unsigned._number)

    override def decimal: Parsley[BigDecimal] = ErrorConfig.label(err.labelRealDecimal)(_decimal)
    override def hexadecimal: Parsley[BigDecimal] = ErrorConfig.label(err.labelRealHexadecimal)(_hexadecimal)
    override def octal: Parsley[BigDecimal] = ErrorConfig.label(err.labelRealOctal)(_octal)
    override def binary: Parsley[BigDecimal] = ErrorConfig.label(err.labelRealBinary)(_binary)
    override def number: Parsley[BigDecimal] = ErrorConfig.label(err.labelRealNumber)(_number)
}

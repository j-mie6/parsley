package parsley.token.numeric

import parsley.Parsley, Parsley.attempt
import parsley.character.{digit, hexDigit, octDigit, oneOf}
import parsley.errors.combinator.ErrorMethods
import parsley.implicits.character.charLift
import parsley.lift.lift2
import parsley.token.descriptions.numeric.NumericDesc

import parsley.internal.deepembedding.singletons
import parsley.internal.deepembedding.Sign.DoubleType

private [token] final class SignedReal(desc: NumericDesc, unsigned: Real) extends Real {
    private val sign = new Parsley(new singletons.Sign[DoubleType.resultType](DoubleType, desc.positiveSign))

    override lazy val decimal: Parsley[BigDecimal] = attempt(sign <*> unsigned.decimal)
    override lazy val hexadecimal: Parsley[BigDecimal] = attempt(sign <*> unsigned.hexadecimal)
    override lazy val octal: Parsley[BigDecimal] = attempt(sign <*> unsigned.octal)
    override lazy val binary: Parsley[BigDecimal] = attempt(sign <*> unsigned.binary)
    override lazy val number: Parsley[BigDecimal] = attempt(sign <*> unsigned.number)
}

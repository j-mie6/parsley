package parsley.token.numeric

import parsley.Parsley, Parsley.attempt
import parsley.character.{digit, hexDigit, octDigit, oneOf}
import parsley.errors.combinator.ErrorMethods
import parsley.implicits.character.charLift
import parsley.lift.lift2

import parsley.internal.deepembedding.singletons
import parsley.internal.deepembedding.Sign.DoubleType

private [token] final class SignedRational(unsigned: Rational) extends Rational {
    private val sign = new Parsley(new singletons.Sign[DoubleType.resultType](DoubleType))

    override def decimal: Parsley[BigDecimal] = attempt(sign <*> unsigned.decimal)
    override def hexadecimal: Parsley[BigDecimal] = attempt(sign <*> unsigned.hexadecimal)
    override def octal: Parsley[BigDecimal] = attempt(sign <*> unsigned.octal)
    override def binary: Parsley[BigDecimal] = attempt(sign <*> unsigned.binary)
    override def number: Parsley[BigDecimal] = attempt(sign <*> unsigned.number)
}

package parsley.token.numeric

import parsley.Parsley, Parsley.{attempt, pure}
import parsley.character.{digit, hexDigit, octDigit, oneOf}
import parsley.errors.combinator.ErrorMethods
import parsley.implicits.character.charLift
import parsley.lift.lift2

import parsley.internal.deepembedding.singletons
import parsley.internal.deepembedding.Sign.DoubleType

private [token] final class UnsignedRational(integer: Integer) extends Rational {
    override lazy val decimal: Parsley[BigDecimal] = attempt(ofRadix(10, 10, digit, oneOf('e', 'E')))
    override lazy val hexadecimal: Parsley[BigDecimal] = attempt('0' *> oneOf('x', 'X') *> ofRadix(16, 2, hexDigit, oneOf('p', 'P')))
    override lazy val octal: Parsley[BigDecimal] = attempt('0' *> oneOf('o', 'O') *> ofRadix(8, 8, octDigit, oneOf('p', 'P')))
    override lazy val binary: Parsley[BigDecimal] = attempt('0' *> oneOf('b', 'B') *> ofRadix(2, 2, oneOf('0', '1'), oneOf('p', 'P')))
    override lazy val number: Parsley[BigDecimal] = hexadecimal <|> octal <|> binary <|> decimal

    private def ofRadix(radix: Int, base: Int, digit: Parsley[Char], exp: Parsley[Char]) = {
        // could allow for foldLeft and foldRight here!
        val whole = digit.foldLeft1[BigDecimal](0)((x, d) => x*radix + d.asDigit)
        val fractional = '.' *> digit.foldRight1[BigDecimal](0)((d, x) => x/radix + d.asDigit)
        val exponent = exp *> integer.decimal32
        val fractExponent =
                (lift2((f: BigDecimal, e: Int) => (w: BigDecimal) => (w + f / radix) * BigDecimal(base).pow(e), fractional, exponent <|> pure(0))
            <|> exponent.map(e => (w: BigDecimal) => w * BigDecimal(base).pow(e)))
        whole <**> fractExponent
    }

}

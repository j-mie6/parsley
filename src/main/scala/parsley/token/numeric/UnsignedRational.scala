package parsley.token.numeric

import parsley.Parsley, Parsley.{attempt, empty, pure}
import parsley.character.{digit, hexDigit, octDigit, oneOf}
import parsley.errors.combinator.ErrorMethods
import parsley.implicits.character.charLift
import parsley.lift.lift2
import parsley.token.descriptions.{NumericDesc, ExponentDesc}

import parsley.internal.deepembedding.singletons
import parsley.internal.deepembedding.Sign.DoubleType

private [token] final class UnsignedRational(desc: NumericDesc, integer: Integer) extends Rational {
    override lazy val decimal: Parsley[BigDecimal] = attempt(ofRadix(10, digit))
    override lazy val hexadecimal: Parsley[BigDecimal] = attempt('0' *> noZeroHexadecimal)
    override lazy val octal: Parsley[BigDecimal] = attempt('0' *> noZeroOctal)
    override lazy val binary: Parsley[BigDecimal] = attempt('0' *> noZeroBinary)
    override lazy val number: Parsley[BigDecimal] = {
        if (desc.decimalRationalsOnly) decimal
        else {
            def addHex(p: Parsley[BigDecimal]) = {
                if (desc.rationalNumbersCanBeHexadecimal) hexadecimal <|> p
                else p
            }
            def addOct(p: Parsley[BigDecimal]) = {
                if (desc.rationalNumbersCanBeOctal) octal <|> p
                else p
            }
            def addBin(p: Parsley[BigDecimal]) = {
                if (desc.rationalNumbersCanBeBinary) binary <|> p
                else p
            }
            // not even accounting for the leading and trailing dot being allowed!
            val zeroLead = '0' *> (addHex(addOct(addBin(ofRadix(10, digit, leadingDotAllowed = true) <|> pure(BigDecimal(0))))))
            attempt(zeroLead <|> decimal)
        }
    }

    // TODO: Using choice here will generate a jump table, which will be nicer for `number` (this requires enhancements to the jumptable optimisation)
    // TODO: Leave these as defs so they get inlined into number for the jumptable optimisation
    private val noZeroHexadecimal = oneOf(desc.hexadecimalLeads) *> ofRadix(16, hexDigit)
    private val noZeroOctal = oneOf(desc.octalLeads) *> ofRadix(8, octDigit)
    private val noZeroBinary = oneOf(desc.binaryLeads) *> ofRadix(2, oneOf('0', '1'))

    // could allow integers to be parsed here according to configuration, the intOrFloat captures that case anyway
    private def ofRadix(radix: Int, digit: Parsley[Char]): Parsley[BigDecimal] = ofRadix(radix, digit, desc.leadingDotAllowed)
    private def ofRadix(radix: Int, digit: Parsley[Char], leadingDotAllowed: Boolean): Parsley[BigDecimal] = {
        val expDesc = desc.exponentDescForRadix(radix)
        // could allow for foldLeft and foldRight here!
        // this reuses components of generic numbers, which will prevent duplication in a larger parser
        val whole = radix match {
            case 10 => integer.plainDecimal
            case 16 => integer.plainHexadecimal
            case 8 => integer.plainOctal
            case 2 => integer.plainBinary
        }
        val fractional = {
            if (desc.trailingDotAllowed) {
                '.' *> digit.foldRight[BigDecimal](0)((d, x) => x/radix + d.asDigit)
            }
            else '.' *> digit.foldRight1[BigDecimal](0)((d, x) => x/radix + d.asDigit)
        }
        val (requiredExponent, exponent, base) = expDesc match {
            case ExponentDesc.Supported(compulsory, exp, base, sign) =>
                // TODO: This integer needs to have the description altered to account for the sign presence
                val exponent = oneOf(exp) *> integer.decimal32
                if (compulsory) (exponent, exponent, base)
                else (exponent, exponent <|> pure(0), base)
            case ExponentDesc.NoExponents => (empty, empty, 0)
        }
        val fractExponent =
                (lift2((f: BigDecimal, e: Int) => (w: BigInt) => (BigDecimal(w) + f / radix) * BigDecimal(base).pow(e),
                       fractional,
                       exponent)
            <|> requiredExponent.map(e => (w: BigInt) => BigDecimal(w) * BigDecimal(base).pow(e)))
        (if (leadingDotAllowed) (whole <|> pure(BigInt(0))) else whole) <**> fractExponent
    }
}

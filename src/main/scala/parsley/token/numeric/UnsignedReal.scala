package parsley.token.numeric

import parsley.Parsley, Parsley.{attempt, empty, pure}
import parsley.character.{digit, hexDigit, octDigit, oneOf}
import parsley.combinator.optional
import parsley.errors.combinator.ErrorMethods
import parsley.implicits.character.charLift
import parsley.lift.lift2
import parsley.token.descriptions.{NumericDesc, ExponentDesc}

import parsley.internal.deepembedding.singletons
import parsley.internal.deepembedding.Sign.DoubleType

private [token] final class UnsignedReal(desc: NumericDesc, natural: Integer) extends Real {
    override lazy val decimal: Parsley[BigDecimal] = attempt(ofRadix(10, digit))
    override lazy val hexadecimal: Parsley[BigDecimal] = attempt('0' *> noZeroHexadecimal)
    override lazy val octal: Parsley[BigDecimal] = attempt('0' *> noZeroOctal)
    override lazy val binary: Parsley[BigDecimal] = attempt('0' *> noZeroBinary)
    override lazy val number: Parsley[BigDecimal] = {
        if (desc.decimalRealsOnly) decimal
        else {
            def addHex(p: Parsley[BigDecimal]) = {
                if (desc.realNumbersCanBeHexadecimal) noZeroHexadecimal <|> p
                else p
            }
            def addOct(p: Parsley[BigDecimal]) = {
                if (desc.realNumbersCanBeOctal) noZeroOctal <|> p
                else p
            }
            def addBin(p: Parsley[BigDecimal]) = {
                if (desc.realNumbersCanBeBinary) noZeroBinary <|> p
                else p
            }
            // this promotes sharing when the definitions would be otherwise equal
            val leadingDotAllowedDecimal = if (desc.leadingDotAllowed) decimal else ofRadix(10, digit, leadingDotAllowed = true)
            // not even accounting for the leading and trailing dot being allowed!
            val zeroLead = '0' *> (addHex(addOct(addBin(leadingDotAllowedDecimal <|> pure(BigDecimal(0))))))
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
        // this reuses components of generic numbers, which will prevent duplication in a larger parser
        val whole = radix match {
            case 10 => natural.plainDecimal
            case 16 => natural.plainHexadecimal
            case 8 => natural.plainOctal
            case 2 => natural.plainBinary
        }
        val f = (d: Char, x: BigDecimal) => x/radix + d.asDigit
        def broken(c: Char) = lift2(f, digit, (optional(c) *> digit).foldRight[BigDecimal](0)(f))
        val fractional = '.' *> {
            desc.literalBreakChar match {
                case None if desc.trailingDotAllowed    => digit.foldRight[BigDecimal](0)(f)
                case None                               => digit.foldRight1[BigDecimal](0)(f)
                case Some(c) if desc.trailingDotAllowed => broken(c) <|> pure[BigDecimal](0)
                case Some(c)                            => broken(c)
            }
        }
        val (requiredExponent, exponent, base) = expDesc match {
            case ExponentDesc.Supported(compulsory, exp, base, sign) =>
                val integer = new SignedInteger(desc.copy(positiveSign = sign), natural)
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

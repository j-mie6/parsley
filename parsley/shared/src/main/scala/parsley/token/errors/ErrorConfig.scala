/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.errors

import parsley.XCompat.unused

import parsley.Parsley, Parsley.pure
import parsley.errors.combinator.{amendThenDislodge, entrench, unexpected, ErrorMethods}
import parsley.position

/** TODO:
  * @since 4.1.0
  */
// TODO: We could make these groupings into merged ADTs? Must be careful with binary-lock-in though...
class ErrorConfig {
    // numeric
    def labelNumericBreakChar: LabelConfig = NotConfigured

    def labelIntegerUnsignedDecimal: LabelConfig = labelIntegerUnsignedNumber
    def labelIntegerUnsignedHexadecimal: LabelConfig = labelIntegerUnsignedNumber
    def labelIntegerUnsignedOctal: LabelConfig = labelIntegerUnsignedNumber
    def labelIntegerUnsignedBinary: LabelConfig = labelIntegerUnsignedNumber
    def labelIntegerUnsignedNumber: LabelConfig = NotConfigured
    def labelIntegerUnsignedDecimal(@unused bits: Int): LabelConfig = labelIntegerUnsignedDecimal
    def labelIntegerUnsignedHexadecimal(@unused bits: Int): LabelConfig = labelIntegerUnsignedHexadecimal
    def labelIntegerUnsignedOctal(@unused bits: Int): LabelConfig = labelIntegerUnsignedOctal
    def labelIntegerUnsignedBinary(@unused bits: Int): LabelConfig = labelIntegerUnsignedBinary
    def labelIntegerUnsignedNumber(@unused bits: Int): LabelConfig = labelIntegerUnsignedNumber

    def labelIntegerSignedDecimal: LabelConfig = labelIntegerSignedNumber
    def labelIntegerSignedHexadecimal: LabelConfig = labelIntegerSignedNumber
    def labelIntegerSignedOctal: LabelConfig = labelIntegerSignedNumber
    def labelIntegerSignedBinary: LabelConfig = labelIntegerSignedNumber
    def labelIntegerSignedNumber: LabelConfig = NotConfigured
    def labelIntegerSignedDecimal(@unused bits: Int): LabelConfig = labelIntegerSignedDecimal
    def labelIntegerSignedHexadecimal(@unused bits: Int): LabelConfig = labelIntegerSignedHexadecimal
    def labelIntegerSignedOctal(@unused bits: Int): LabelConfig = labelIntegerSignedOctal
    def labelIntegerSignedBinary(@unused bits: Int): LabelConfig = labelIntegerSignedBinary
    def labelIntegerSignedNumber(@unused bits: Int): LabelConfig = labelIntegerSignedNumber

    def labelIntegerDecimalEnd: LabelConfig = labelIntegerNumberEnd
    def labelIntegerHexadecimalEnd: LabelConfig = labelIntegerNumberEnd
    def labelIntegerOctalEnd: LabelConfig = labelIntegerNumberEnd
    def labelIntegerBinaryEnd: LabelConfig = labelIntegerNumberEnd
    def labelIntegerNumberEnd: LabelConfig = NotConfigured

    def labelRealDecimal: LabelConfig = labelRealNumber
    def labelRealHexadecimal: LabelConfig = labelRealNumber
    def labelRealOctal: LabelConfig = labelRealNumber
    def labelRealBinary: LabelConfig = labelRealNumber
    def labelRealNumber: LabelConfig = NotConfigured
    def labelRealFloatDecimal: LabelConfig = labelRealDecimal
    def labelRealFloatHexadecimal: LabelConfig = labelRealHexadecimal
    def labelRealFloatOctal: LabelConfig = labelRealOctal
    def labelRealFloatBinary: LabelConfig = labelRealBinary
    def labelRealFloatNumber: LabelConfig = labelRealNumber
    def labelRealDoubleDecimal: LabelConfig = labelRealDecimal
    def labelRealDoubleHexadecimal: LabelConfig = labelRealHexadecimal
    def labelRealDoubleOctal: LabelConfig = labelRealOctal
    def labelRealDoubleBinary: LabelConfig = labelRealBinary
    def labelRealDoubleNumber: LabelConfig = labelRealNumber

    def labelRealDecimalEnd: LabelConfig = labelRealNumberEnd
    def labelRealHexadecimalEnd: LabelConfig = labelRealNumberEnd
    def labelRealOctalEnd: LabelConfig = labelRealNumberEnd
    def labelRealBinaryEnd: LabelConfig = labelRealNumberEnd
    def labelRealNumberEnd: LabelConfig = NotConfigured

    def labelRealDot: LabelConfig = NotConfigured
    def labelRealExponent: LabelConfig = NotConfigured
    def labelRealExponentEnd: LabelConfig = NotConfigured

    private [token] final def labelDecimal(bits: Int, signed: Boolean): LabelConfig = {
        if (signed) labelIntegerSignedDecimal(bits) else labelIntegerUnsignedDecimal(bits)
    }
    private [token] final def labelHexadecimal(bits: Int, signed: Boolean): LabelConfig = {
        if (signed) labelIntegerSignedHexadecimal(bits) else labelIntegerUnsignedHexadecimal(bits)
    }
    private [token] final def labelOctal(bits: Int, signed: Boolean): LabelConfig = {
        if (signed) labelIntegerSignedOctal(bits) else labelIntegerUnsignedOctal(bits)
    }
    private [token] final def labelBinary(bits: Int, signed: Boolean): LabelConfig = {
        if (signed) labelIntegerSignedBinary(bits) else labelIntegerUnsignedBinary(bits)
    }
    private [token] final def labelNumber(bits: Int, signed: Boolean): LabelConfig = {
        if (signed) labelIntegerSignedNumber(bits) else labelIntegerUnsignedNumber(bits)
    }

    def explainNumericBreakChar: Option[String] = None

    def explainRealNoDoubleDroppedZero: String =
        "a real number cannot drop both a leading and trailing zero"

    // TODO: render in the "native" radix
    def messageIntTooLarge(n: BigInt, max: BigInt, @unused nativeRadix: Int): Seq[String] =
        Seq(s"literal $n is larger than max value of $max")

    // TODO: render in the "native" radix
    def messageIntTooSmall(n: BigInt, min: BigInt, @unused nativeRadix: Int): Seq[String] =
        Seq(s"literal $n is less than min value of $min")

    def messageRealNotExact(n: BigDecimal, name: String): Seq[String] =
        Seq(s"literal $n cannot be represented exactly as an $name")

    def messageRealTooLarge(n: BigDecimal, name: String): Seq[String] =
        Seq(s"literal $n is too large to be an $name")

    def messageRealTooSmall(n: BigDecimal, name: String): Seq[String] =
        Seq(s"literal $n is too small to be an $name")

    // expensive ;)
    // this is not as effective as it may seem, because reasons cannot be hints
    // it's possible a preventative error could be more effective?
    /*def verifiedIntegerBadCharsUsedInLiteral: Option[(predicate.CharPredicate, Int => String)] =
        None*/

    def doubleName: String = "IEEE 754 double-precision float"
    def floatName: String = "IEEE 754 single-precision float"

    // names
    def labelNameIdentifier: String = "identifier"
    def labelNameOperator: String = "operator"
    def unexpectedNameIllegalIdentifier(v: String): String = s"keyword $v"
    def unexpectedNameIllegalOperator(v: String): String = s"reserved operator $v"
    def unexpectedNameIllFormedIdentifier: Option[String => String] = Some(v => s"identifer $v")
    def unexpectedNameIllFormedOperator: Option[String => String] = Some(v => s"operator $v")

    // text
    def labelCharAscii: LabelConfig = NotConfigured
    def labelCharLatin1: LabelConfig = NotConfigured
    def labelCharBasicMultilingualPlane: LabelConfig = NotConfigured
    def labelCharUtf16: LabelConfig = NotConfigured

    def labelCharAsciiEnd: LabelConfig = NotConfigured
    def labelCharLatin1End: LabelConfig = NotConfigured
    def labelCharBasicMultilingualPlaneEnd: LabelConfig = NotConfigured
    def labelCharUtf16End: LabelConfig = NotConfigured

    def labelStringAscii(multi: Boolean, raw: Boolean): LabelConfig = NotConfigured
    def labelStringLatin1(multi: Boolean, raw: Boolean): LabelConfig = NotConfigured
    def labelStringUtf16(multi: Boolean, raw: Boolean): LabelConfig = NotConfigured

    def labelStringAsciiEnd(multi: Boolean, raw: Boolean): LabelConfig = NotConfigured
    def labelStringLatin1End(multi: Boolean, raw: Boolean): LabelConfig = NotConfigured
    def labelStringUtf16End(multi: Boolean, raw: Boolean): LabelConfig = NotConfigured

    def labelStringCharacter: LabelConfig = Label("string character")
    def labelGraphicCharacter: LabelConfig = Label("graphic character")
    def labelEscapeSequence: LabelConfig = Label("escape sequence")
    def labelEscapeNumeric(radix: Int): LabelConfig = NotConfigured
    def labelEscapeNumericEnd(radix: Int): LabelConfig = NotConfigured
    def labelEscapeEnd: LabelConfig = Label("end of escape sequence")
    def labelStringEscapeEmpty: LabelConfig = NotConfigured
    def labelStringEscapeGap: LabelConfig = Label("string gap")
    def labelStringEscapeGapEnd: LabelConfig = Label("end of string gap")

    def unexpectedCharNonBasicMultilingualPlane: Option[Int => String] = None
    def unexpectedCharNonAscii: Option[Int => String] = None
    def unexpectedCharNonLatin1: Option[Int => String] = None

    def explainCharNonBasicMultilingualPlane: Option[Int => String] = Some(_ => "non-BMP character")
    def explainCharNonAscii: Option[Int => String] = Some(_ => "non-ascii character")
    def explainCharNonLatin1: Option[Int => String] = Some(_ => "non-latin1 character")
    def explainGraphicCharacter: Option[String] = None

    def explainEscapeInvalid: Option[String] =
        Some("invalid escape sequence")
    def explainEscapeNumericPostPrefix(prefix: Char, radix: Int): Option[String] = None

    def messageStringNonAscii(@unused s: String): Seq[String] =
        Seq("non-ascii characters in string literal, this is not allowed")

    def messageStringNonLatin1(@unused s: String): Seq[String] =
        Seq("non-latin1 characters in string literal, this is not allowed")

    def messageEscapeCharRequiresExactDigits(@unused radix: Int, got: Int, needed: Seq[Int]): Seq[String] =
        Seq(s"numeric escape requires ${parsley.errors.helpers.combineAsList(needed.toList.map(_.toString))} digits, but only got $got")

    def messageEscapeCharNumericSequenceTooBig(escapeSequence: String, maxEscape: String): Seq[String] =
        Seq(s"$escapeSequence is greater than the maximum character $maxEscape")

    def messageEscapeCharNumericSequenceIllegal(escapeSequence: String): Seq[String] =
        Seq(s"illegal unicode codepoint: $escapeSequence")

    private [parsley] def renderEscapeNumericSequence(escBegin: Char, prefix: String, n: BigInt, radix: Int): String =
        s"$escBegin$prefix${n.toString(radix)}"

    // expensive ;)
    def verifiedCharBadCharsUsedInLiteral: Map[Int, String] = Map.empty
    def verifiedStringBadCharsUsedInLiteral: Map[Int, String] = Map.empty

    // symbol
    def labelSymbolSemi: LabelConfig = Label("semicolon")
    def labelSymbolComma: LabelConfig = Label("comma")
    def labelSymbolColon: LabelConfig = Label("colon")
    def labelSymbolDot: LabelConfig = Label("dot")
    def labelSymbolOpenParen: LabelConfig = Label("open parenthesis")
    def labelSymbolOpenBrace: LabelConfig = Label("open brace")
    def labelSymbolOpenSquare: LabelConfig = Label("open square bracket")
    def labelSymbolOpenAngle: LabelConfig = Label("open angle bracket")
    def labelSymbolClosingParen: LabelConfig = Label("closing parenthesis")
    def labelSymbolClosingBrace: LabelConfig = Label("closing brace")
    def labelSymbolClosingSquare: LabelConfig = Label("closing square bracket")
    def labelSymbolClosingAngle: LabelConfig = Label("closing angle bracket")
    def labelSymbolKeyword(symbol: String): LabelConfig = Label(symbol)
    def labelSymbolOperator(symbol: String): LabelConfig = Label(symbol)
    def labelSymbolEndOfKeyword(symbol: String): String = s"end of $symbol"
    def labelSymbolEndOfOperator(symbol: String): String = s"end of $symbol"

    // space
    def labelSpaceEndOfLineComment: LabelConfig = Label("end of comment")
    def labelSpaceEndOfMultiComment: LabelConfig = Label("end of comment")
}

private [token] object ErrorConfig {
    private [token] def explain[A](reason: Option[String])(p: Parsley[A]): Parsley[A] = reason match {
        case None => p
        case Some(reason) => p.explain(reason)
    }

    private [token] def unexpectedWhenWithReason[A](pred: A => Boolean, unexGen: Option[A => String], reasonGen: Option[A => String])(p: Parsley[A]) = {
        unexGen match {
            case None => reasonGen match {
                case None => p.filterNot(pred)
                case Some(g) => p.filterOut { case x if pred(x) => g(x) }
            }
            case Some(g1) => reasonGen match {
                case None => p.unexpectedWhen { case x if pred(x) => g1(x) }
                case Some(g2) => amendThenDislodge {
                    position.internalOffsetSpan(entrench(p)).flatMap { case (os, x, oe) =>
                        if (pred(x)) unexpected(oe - os, g1(x)).explain(g2(x))
                        else pure(x)
                    }
                }
            }
        }
    }
}

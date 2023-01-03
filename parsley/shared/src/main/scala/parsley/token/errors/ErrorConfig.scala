/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.errors // TODO: move out of this package?

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
    def labelIntegerUnsignedDecimal: Option[String] = None
    def labelIntegerUnsignedHexadecimal: Option[String] = None
    def labelIntegerUnsignedOctal: Option[String] = None
    def labelIntegerUnsignedBinary: Option[String] = None
    def labelIntegerUnsignedNumber: Option[String] = None
    def labelIntegerUnsignedDecimal(@unused bits: Int): Option[String] = None
    def labelIntegerUnsignedHexadecimal(@unused bits: Int): Option[String] = None
    def labelIntegerUnsignedOctal(@unused bits: Int): Option[String] = None
    def labelIntegerUnsignedBinary(@unused bits: Int): Option[String] = None
    def labelIntegerUnsignedNumber(@unused bits: Int): Option[String] = None

    def labelIntegerSignedDecimal: Option[String] = None
    def labelIntegerSignedHexadecimal: Option[String] = None
    def labelIntegerSignedOctal: Option[String] = None
    def labelIntegerSignedBinary: Option[String] = None
    def labelIntegerSignedNumber: Option[String] = None
    def labelIntegerSignedDecimal(@unused bits: Int): Option[String] = None
    def labelIntegerSignedHexadecimal(@unused bits: Int): Option[String] = None
    def labelIntegerSignedOctal(@unused bits: Int): Option[String] = None
    def labelIntegerSignedBinary(@unused bits: Int): Option[String] = None
    def labelIntegerSignedNumber(@unused bits: Int): Option[String] = None

    def labelIntegerDecimalEnd: Option[String] = Some("end of number")
    def labelIntegerHexadecimalEnd: Option[String] = Some("end of number")
    def labelIntegerOctalEnd: Option[String] = Some("end of number")
    def labelIntegerBinaryEnd: Option[String] = Some("end of number")

    private [token] def labelDecimal(bits: Int, signed: Boolean): Option[String] = {
        if (signed) labelIntegerSignedDecimal(bits) else labelIntegerUnsignedDecimal(bits)
    }
    private [token] def labelHexadecimal(bits: Int, signed: Boolean): Option[String] = {
        if (signed) labelIntegerSignedHexadecimal(bits) else labelIntegerUnsignedHexadecimal(bits)
    }
    private [token] def labelOctal(bits: Int, signed: Boolean): Option[String] = {
        if (signed) labelIntegerSignedOctal(bits) else labelIntegerUnsignedOctal(bits)
    }
    private [token] def labelBinary(bits: Int, signed: Boolean): Option[String] = {
        if (signed) labelIntegerSignedBinary(bits) else labelIntegerUnsignedBinary(bits)
    }
    private [token] def labelNumber(bits: Int, signed: Boolean): Option[String] = {
        if (signed) labelIntegerSignedNumber(bits) else labelIntegerUnsignedNumber(bits)
    }

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
    def labelCharAscii: Option[String] = None
    def labelCharLatin1: Option[String] = None
    def labelCharBasicMultilingualPlane: Option[String] = None
    def labelCharUtf16: Option[String] = None

    def labelCharAsciiEnd: Option[String] = None
    def labelCharLatin1End: Option[String] = None
    def labelCharBasicMultilingualPlaneEnd: Option[String] = None
    def labelCharUtf16End: Option[String] = None

    def labelStringAscii(multi: Boolean, raw: Boolean): Option[String] = None
    def labelStringLatin1(multi: Boolean, raw: Boolean): Option[String] = None
    def labelStringUtf16(multi: Boolean, raw: Boolean): Option[String] = None

    def labelStringAsciiEnd(multi: Boolean, raw: Boolean): Option[String] = None
    def labelStringLatin1End(multi: Boolean, raw: Boolean): Option[String] = None
    def labelStringUtf16End(multi: Boolean, raw: Boolean): Option[String] = None

    def labelStringCharacter: Option[String] = Some("string character")
    def labelGraphicCharacter: Option[String] = Some("graphic character")
    def labelEscapeSequence: Option[String] = Some("escape sequence")
    def labelEscapeNumeric(radix: Int): Option[String] = None
    def labelEscapeNumericEnd(radix: Int): Option[String] = None
    def labelEscapeEnd: Option[String] = Some("end of escape sequence")
    def labelStringEscapeEmpty: Option[String] = None
    def labelStringEscapeGap: Option[String] = Some("string gap")
    def labelStringEscapeGapEnd: Option[String] = Some("end of string gap")

    def unexpectedCharNonBasicMultilingualPlane: Option[Int => String] = None
    def unexpectedCharNonAscii: Option[Int => String] = None
    def unexpectedCharNonLatin1: Option[Int => String] = None

    def explainCharNonBasicMultilingualPlane: Option[Int => String] = Some(_ => "non-BMP character")
    def explainCharNonAscii: Option[Int => String] = Some(_ => "non-ascii character")
    def explainCharNonLatin1: Option[Int => String] = Some(_ => "non-latin1 character")

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
    def labelSymbolSemi: Option[String] = Some("semicolon")
    def labelSymbolComma: Option[String] = Some("comma")
    def labelSymbolColon: Option[String] = Some("colon")
    def labelSymbolDot: Option[String] = Some("dot")
    def labelSymbolOpenParen: Option[String] = Some("open parenthesis")
    def labelSymbolOpenBrace: Option[String] = Some("open brace")
    def labelSymbolOpenSquare: Option[String] = Some("open square bracket")
    def labelSymbolOpenAngle: Option[String] = Some("open angle bracket")
    def labelSymbolClosingParen: Option[String] = Some("closing parenthesis")
    def labelSymbolClosingBrace: Option[String] = Some("closing brace")
    def labelSymbolClosingSquare: Option[String] = Some("closing square bracket")
    def labelSymbolClosingAngle: Option[String] = Some("closing angle bracket")
    def labelSymbolKeyword(symbol: String): Option[String] = Some(symbol)
    def labelSymbolOperator(symbol: String): Option[String] = Some(symbol)
    def labelSymbolEndOfKeyword(symbol: String): String = s"end of $symbol"
    def labelSymbolEndOfOperator(symbol: String): String = s"end of $symbol"

    // space
    //def labelSpaceComment: Option[String] = Some("comment")
    def labelSpaceEndOfLineComment: Option[String] = Some("end of comment")
    def labelSpaceEndOfMultiComment: Option[String] = Some("end of comment")
    // TODO: explainSpaceUnclosedComment?
}

object ErrorConfig {
    val default = new ErrorConfig

    private [token] def label[A](label: Option[String])(p: Parsley[A]): Parsley[A] = label match {
        case None => p
        case Some(name) => p.label(name)
    }

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

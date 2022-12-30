/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.errors // TODO: move out of this package?

import parsley.XCompat.unused

import parsley.Parsley
import parsley.errors.combinator.ErrorMethods

/** TODO:
  *
  * @since 4.1.0
  */
class ErrorConfig {
    // numeric
    private [token] def explainRealNoDoubleDroppedZero: String =
        "a real number cannot drop both a leading and trailing zero"

    // TODO: render in the "native" radix
    private [token] def messageIntTooLarge(n: BigInt, max: BigInt, @unused nativeRadix: Int): Seq[String] =
        Seq(s"literal $n is larger than max value of $max")

    // TODO: render in the "native" radix
    private [token] def messageIntTooSmall(n: BigInt, min: BigInt, @unused nativeRadix: Int): Seq[String] =
        Seq(s"literal $n is less than min value of $min")

    private [token] def messageRealNotExact(n: BigDecimal, name: String): Seq[String] =
        Seq(s"literal $n cannot be represented exactly as an $name")

    private [token] def messageRealTooLarge(n: BigDecimal, name: String): Seq[String] =
        Seq(s"literal $n is too large to be an $name")

    private [token] def messageRealTooSmall(n: BigDecimal, name: String): Seq[String] =
        Seq(s"literal $n is too small to be an $name")

    private [token] def doubleName: String = "IEEE 754 double-precision float"
    private [token] def floatName: String = "IEEE 754 single-precision float"

    // names
    private [token] def labelNameIdentifier: String = "identifier"
    private [token] def labelNameOperator: String = "operator"
    private [token] def unexpectedNameIllegalIdentifier(v: String): String = s"keyword $v"
    private [token] def unexpectedNameIllegalOperator(v: String): String = s"reserved operator $v"
    private [token] def unexpectedNameIllFormedIdentifier: Option[String => String] = Some(v => s"identifer $v")
    private [token] def unexpectedNameIllFormedOperator: Option[String => String] = Some(v => s"operator $v")

    // text
    //private [token] def labelCharAscii: Option[String] = None
    private [token] def labelStringCharacter: Option[String] = Some("string character")
    private [token] def labelStringCharacterGraphic: Option[String] = Some("graphic character")
    private [token] def labelEscapeSequnce: Option[String] = Some("escape sequence")
    private [token] def labelEscapeEnd: Option[String] = Some("end of escape sequence")
    private [token] def labelEscapeStringGap: Option[String] = Some("string gap")
    private [token] def labelEscapeStringGapEnd: Option[String] = Some("end of string gap")

    private [token] def explainCharNonAscii(@unused c: Int): String =
        "non-ascii character"

    private [token] def explainCharNonLatin1(@unused c: Int): String =
        "non-latin1 character"
    private [token] def explainEscapeInvalid: String =
        "invalid escape sequence"

    //private [token] def explainCharNonBasicMultilingualPlane(@unused c: Int): String =
    //    "non-BMP character"

    //private [token] def messageCharEscapeNonAscii(@unused c: Int): Seq[String] =
    //    Seq("non-ascii character")

    //private [token] def messageCharEscapeNonLatin1(@unused c: Int): Seq[String] =
    //    Seq("non-latin1 character")

    private [token] def messageCharEscapeNonBasicMultilingualPlane(@unused c: Int): Seq[String] =
        Seq("non-BMP character")

    private [token] def messageStringNonAscii(@unused s: String): Seq[String] =
        Seq("non-ascii characters in string literal, this is not allowed")

    private [token] def messageStringNonLatin1(@unused s: String): Seq[String] =
        Seq("non-latin1 characters in string literal, this is not allowed")

    private [token] def messageCharEscapeRequiresExactDigits(@unused radix: Int, got: Int, needed: Seq[Int]): Seq[String] =
        Seq(s"numeric escape requires ${parsley.errors.helpers.combineAsList(needed.toList.map(_.toString))} digits, but only got $got")

    private [token] def messageCharEscapeNumericSequenceTooBig(escapeSequence: String, maxEscape: String): Seq[String] =
        Seq(s"$escapeSequence is greater than the maximum character $maxEscape")

    private [token] def messageCharEscapeNumericSequenceIllegal(escapeSequence: String): Seq[String] =
        Seq(s"illegal unicode codepoint: $escapeSequence")

    private [token] def renderCharEscapeNumericSequence(escBegin: Char, prefix: String, n: BigInt, radix: Int): String =
        s"$escBegin$prefix${n.toString(radix)}"

    // symbol
    private [token] def labelSymbolSemi: Option[String] = Some("semicolon")
    private [token] def labelSymbolComma: Option[String] = Some("comma")
    private [token] def labelSymbolColon: Option[String] = Some("colon")
    private [token] def labelSymbolDot: Option[String] = Some("dot")
    private [token] def labelSymbolOpenParen: Option[String] = Some("open parenthesis")
    private [token] def labelSymbolOpenBrace: Option[String] = Some("open brace")
    private [token] def labelSymbolOpenSquare: Option[String] = Some("open square bracket")
    private [token] def labelSymbolOpenAngle: Option[String] = Some("open angle bracket")
    private [token] def labelSymbolClosingParen: Option[String] = Some("closing parenthesis")
    private [token] def labelSymbolClosingBrace: Option[String] = Some("closing brace")
    private [token] def labelSymbolClosingSquare: Option[String] = Some("closing square bracket")
    private [token] def labelSymbolClosingAngle: Option[String] = Some("closing angle bracket")
    private [token] def labelSymbolKeyword(symbol: String): Option[String] = Some(symbol)
    private [token] def labelSymbolOperator(symbol: String): Option[String] = Some(symbol)
    private [token] def labelSymbolEndOfKeyword(symbol: String): String = s"end of $symbol"
    private [token] def labelSymbolEndOfOperator(symbol: String): String = s"end of $symbol"

    // space
    //private [parsley] def labelSpaceComment: Option[String] = Some("comment")
    private [parsley] def labelSpaceEndOfLineComment: Option[String] = Some("end of comment")
    private [parsley] def labelSpaceEndOfMultiComment: Option[String] = Some("end of comment")
    // TODO: reasonSpaceUnclosedComment?
}

object ErrorConfig {
    val default = new ErrorConfig

    private [token] def label[A](label: Option[String])(p: Parsley[A]): Parsley[A] = label match {
        case None => p
        case Some(name) => p.label(name)
    }
}

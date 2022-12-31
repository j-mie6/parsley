/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.errors // TODO: move out of this package?

import parsley.XCompat.unused

import parsley.Parsley
import parsley.errors.combinator.ErrorMethods

/** TODO:
  * @since 4.1.0
  */
class ErrorConfig {
    // numeric
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
    //def labelCharAscii: Option[String] = None
    def labelStringCharacter: Option[String] = Some("string character")
    def labelStringCharacterGraphic: Option[String] = Some("graphic character")
    def labelEscapeSequence: Option[String] = Some("escape sequence")
    def labelEscapeNumeric: Option[String] = None
    def labelEscapeEnd: Option[String] = Some("end of escape sequence")
    def labelEscapeStringGap: Option[String] = Some("string gap")
    def labelEscapeStringGapEnd: Option[String] = Some("end of string gap")

    def explainCharNonAscii(@unused c: Int): String =
        "non-ascii character"

    def explainCharNonLatin1(@unused c: Int): String =
        "non-latin1 character"
    def explainEscapeInvalid: String =
        "invalid escape sequence"

    def explainEscapeEnd: Option[String] = None

    //def explainCharNonBasicMultilingualPlane(@unused c: Int): String =
    //    "non-BMP character"

    //def messageCharEscapeNonAscii(@unused c: Int): Seq[String] =
    //    Seq("non-ascii character")

    //def messageCharEscapeNonLatin1(@unused c: Int): Seq[String] =
    //    Seq("non-latin1 character")

    def messageCharEscapeNonBasicMultilingualPlane(@unused c: Int): Seq[String] =
        Seq("non-BMP character")

    def messageStringNonAscii(@unused s: String): Seq[String] =
        Seq("non-ascii characters in string literal, this is not allowed")

    def messageStringNonLatin1(@unused s: String): Seq[String] =
        Seq("non-latin1 characters in string literal, this is not allowed")

    def messageCharEscapeRequiresExactDigits(@unused radix: Int, got: Int, needed: Seq[Int]): Seq[String] =
        Seq(s"numeric escape requires ${parsley.errors.helpers.combineAsList(needed.toList.map(_.toString))} digits, but only got $got")

    def messageCharEscapeNumericSequenceTooBig(escapeSequence: String, maxEscape: String): Seq[String] =
        Seq(s"$escapeSequence is greater than the maximum character $maxEscape")

    def messageCharEscapeNumericSequenceIllegal(escapeSequence: String): Seq[String] =
        Seq(s"illegal unicode codepoint: $escapeSequence")

    private [parsley] def renderCharEscapeNumericSequence(escBegin: Char, prefix: String, n: BigInt, radix: Int): String =
        s"$escBegin$prefix${n.toString(radix)}"

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
    // TODO: reasonSpaceUnclosedComment?
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
}

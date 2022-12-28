/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.errors // TODO: move out of this package?

import scala.annotation.unused

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

    // symbol

    // space
}

object ErrorConfig {
    val default = new ErrorConfig

    private [token] def label[A](label: Option[String])(p: Parsley[A]): Parsley[A] = label match {
        case None => p
        case Some(name) => p.label(name)
    }
}

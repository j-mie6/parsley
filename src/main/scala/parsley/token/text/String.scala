/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.text

import scala.Predef.{String => ScalaString, _}

import parsley.Parsley
import parsley.errors.combinator.{amend, entrench, ErrorMethods}
import parsley.XCompat

/** This class defines a uniform interface for defining parsers for string
  * literals, independent of whether the string is raw, multi-line, or should
  * consume whitespace after the literal.
  *
  * @since 4.0.0
  * @note implementations of this class found within `Lexer` may employ sharing
  *       and refine the `def`s in this class into `val` or `lazy val` when overriding.
  *
  * @define disclaimer
  *   the exact behaviour of this parser is decided by the implementations given in
  *   `Lexer`, which will depend on user-defined configuration. Please see the
  *   relevant documentation of these specific objects.
  */
abstract class String private[token] {
    /** This parser will parse a single string literal, which may contain any
      * number of graphical UTF-16 unicode characters; including those that span multiple
      * 32 bit codepoints. It may contain escape sequences, and potentially
      * support string gaps and zero-width characters depending on the
      * configuration.
      *
      * @todo TODO: examples
      * @since 4.0.0
      * @note $disclaimer
      */
    def fullUtf16: Parsley[ScalaString]
    /** This parser will parse a single string literal, which may contain any
      * number of graphic extended ascii characters (known as latin1). It may contain escape
      * sequences, and potentially support string gaps and zero-width characters
      * depending on the configuration.
      *
      * @todo TODO: examples
      * @since 4.0.0
      * @note $disclaimer
      */
    def latin1: Parsley[ScalaString]
    /** This parser will parse a single string literal, which may contain any
      * number of graphic ascii characters. It may contain escape
      * sequences, and potentially support string gaps and zero-width characters
      * depending on the configuration.
      *
      * @todo TODO: examples
      * @since 4.0.0
      * @note $disclaimer
      */
    def ascii: Parsley[ScalaString]
}

private [text] object String {
    private def allCharsWithin(str: ScalaString, bound: Int) = XCompat.codePoints(str).forall(_ <= bound)
    def isAscii(str: ScalaString): Boolean = allCharsWithin(str, Character.MaxAscii)
    def isExtendedAscii(str: ScalaString): Boolean = allCharsWithin(str, Character.MaxLatin1)

    def ensureAscii(p: Parsley[ScalaString]): Parsley[ScalaString] = amend {
        entrench(p).guardAgainst {
            case str if !isAscii(str) => Seq("non-ascii characters in string literal, this is not allowed")
       }
    }

    def ensureExtendedAscii(p: Parsley[ScalaString]): Parsley[ScalaString] = amend {
        entrench(p).guardAgainst {
            case str if !isExtendedAscii(str) => Seq("non-extended-ascii characters in string literal, this is not allowed")
       }
    }
}

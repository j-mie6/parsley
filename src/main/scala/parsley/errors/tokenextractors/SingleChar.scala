/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.errors.tokenextractors

import parsley.errors.{helpers, ErrorBuilder, Token, TokenSpan}

// Turn coverage off, because the tests have their own error builder
// We might want to test this on its own though
// $COVERAGE-OFF$
/** This extractor mixin provides an implementation for
  * [[parsley.errors.ErrorBuilder.unexpectedToken `ErrorBuilder.unexpectedToken`]] when mixed into
  * an error builder: it will unconditionally report the first character in the remaining input
  * as the problematic token.
  * @since 4.0.0
  * @note In the case of unprintable characters or whitespace, this extractor will favour reporting
  *       a more meaningful name.
  */
trait SingleChar { this: ErrorBuilder[_] =>
    /** @see [[parsley.errors.ErrorBuilder.unexpectedToken `ErrorBuilder.unexpectedToken`]] */
    override final def unexpectedToken(cs: IndexedSeq[Char], amountOfInputParserWanted: Int, lexicalError: Boolean): Token = SingleChar.unexpectedToken(cs)
}

/** Contains the functionality of `SingleChar` as a function.
  * @since 4.0.0
  */
object SingleChar {
    /** The implementation of `unexpectedToken` as done by `SingleChar`, with redundant arguments removed.
      * @since 4.0.0
      */
    def unexpectedToken(cs: IndexedSeq[Char]): Token = cs.head match {
        case helpers.WhitespaceOrUnprintable(name) => Token.Named(name, TokenSpan.Width(1))
        case c if c.isHighSurrogate => Token.Raw(cs.take(2).mkString)
        case c => Token.Raw(s"$c")
    }
}
// $COVERAGE-ON$

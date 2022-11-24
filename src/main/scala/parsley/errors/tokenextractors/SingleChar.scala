/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.errors.tokenextractors

import parsley.errors.{helpers, ErrorBuilder, Token, TokenSpan}

// Turn coverage off, because the tests have their own error builder
// We might want to test this on its own though
// $COVERAGE-OFF$
trait SingleChar { this: ErrorBuilder[_] =>
    override def unexpectedToken(cs: IndexedSeq[Char], amountOfInputParserWanted: Int, lexicalError: Boolean): Token = cs.head match {
        case helpers.WhitespaceOrUnprintable(name) => Token.Named(name, TokenSpan.Width(1))
        case c if c.isHighSurrogate => Token.Raw(cs.take(2).mkString)
        case c => Token.Raw(s"$c")
    }
}
// $COVERAGE-ON$

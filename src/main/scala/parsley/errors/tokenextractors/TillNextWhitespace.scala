/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.errors.tokenextractors

import parsley.errors.{ErrorBuilder, helpers, Named, Raw, Token, Width}

import scala.collection.immutable.WrappedString

// Turn coverage off, because the tests have their own error builder
// We might want to test this on its own though
// $COVERAGE-OFF$
trait TillNextWhitespace { this: ErrorBuilder[_] =>
    def trimToParserDemand: Boolean

    override def unexpectedToken(cs: Iterable[Char], amountOfInputParserWanted: Int): Token = {
      cs match {
        case helpers.WhitespaceOrUnprintable(name) => Named(name, Width(1))
        case cs: WrappedString =>
            // These do not require allocation on the string
            val idx = cs.indexWhere(_.isWhitespace)
            val tok = if (idx != -1) cs.slice(0, idx) else cs
            Raw(trim(tok.toString, amountOfInputParserWanted))
        case cs => Raw(trim(cs.takeWhile(!_.isWhitespace).mkString, amountOfInputParserWanted))
      }
    }

    private def trim(s: String, amountOfInputParserWanted: Int): String = {
        if (trimToParserDemand) s.slice(0, amountOfInputParserWanted)
        else s
    }
}
// $COVERAGE-ON$

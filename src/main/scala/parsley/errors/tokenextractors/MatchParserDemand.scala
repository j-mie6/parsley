/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.errors.tokenextractors

import scala.collection.immutable.WrappedString

import parsley.errors.{helpers, ErrorBuilder, Named, Raw, Token, Width}

// Turn coverage off, because the tests have their own error builder
// We might want to test this on its own though
// $COVERAGE-OFF$
trait MatchParserDemand { this: ErrorBuilder[_] =>
    override def unexpectedToken(cs: Iterable[Char], amountOfInputParserWanted: Int): Token = {
      cs match {
        case helpers.WhitespaceOrUnprintable(name) => Named(name, Width(1))
        // the default case will build a new string, if the underlying was already a string
        // this is redundant.
        case cs: WrappedString => Raw(cs.slice(0, amountOfInputParserWanted).toString)
        case _                 => Raw(cs.take(amountOfInputParserWanted).mkString)
      }
    }
}
// $COVERAGE-ON$

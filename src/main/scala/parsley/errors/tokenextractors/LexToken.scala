/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.errors.tokenextractors

import parsley.Parsley, Parsley.{attempt, col, lookAhead}
import parsley.Success
import parsley.character.{noneOf, newline}
import parsley.combinator.{eof, manyUntil, option, sequence}
import parsley.errors.{ErrorBuilder, helpers, Named, Raw, Token}

import scala.collection.immutable.WrappedString

// Turn coverage off, because the tests have their own error builder
// We might want to test this on its own though
// $COVERAGE-OFF$
// TODO: The problem with this is that it shouldn't apply the heuristics to lexing errors, only parsing errors
// We'd need a way of signalling that from the lexer itself?
trait LexToken { this: ErrorBuilder[_] =>

    def tokens: Seq[Parsley[String]]
    def whitespace: Parsley[_]

    private def makeParser: Parsley[(String, List[(String, Int)])] = {
        // we start at column 1 anyway
        val trueCol = col.map(_ - 1)
        // cannot fail
        val tokWidth = lookAhead(manyUntil(noneOf('\n'), whitespace <|> eof <|> newline).map(_.mkString))
        tokWidth <~> sequence(tokens.map(p => option(attempt(lookAhead(p <~> trueCol)))): _*).map(_.flatten)
    }

    def selectToken(maxWidth: Int, rawToken: String, matchedToks: List[(String, Int)]): Token = {
        val toks = matchedToks.sortBy(_._2).map {
            case (name, width) => Named(name, Math.max(width, maxWidth))
        }
        toks.lastOption.getOrElse(Raw(rawToken))
    }

    override def unexpectedToken(cs: Iterable[Char], amountOfInputParserWanted: Int): Token = {
        val Success((rawToken, matchedToks)) = makeParser.parse {
            cs match {
                case cs: WrappedString => cs.toString
                case cs => cs.mkString
            }
        }
        // TODO: not really ideal, what about parsing comments etc?
        if (rawToken.isEmpty) Named("whitespace", 1)
        else selectToken(rawToken.length, rawToken, matchedToks)
    }
}

object LexToken {
    def constantNames(ps: (Parsley[_], String)*): Seq[Parsley[String]] = ps.map {
        case (p, n) => p #> n
    }
}
// $COVERAGE-ON$

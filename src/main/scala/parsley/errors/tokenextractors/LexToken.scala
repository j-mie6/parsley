/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.errors.tokenextractors

import parsley.Parsley, Parsley.{attempt, lookAhead}
import parsley.Success
import parsley.character.{noneOf, newline, item}
import parsley.combinator.{eof, manyUntil, option, sequence}
import parsley.errors.{ErrorBuilder, helpers, Named, Raw, Token, UntilPos, Width}

import scala.collection.immutable.WrappedString

// Turn coverage off, because the tests have their own error builder
// We might want to test this on its own though
// $COVERAGE-OFF$
// TODO: The problem with this is that it shouldn't apply the heuristics to lexing errors, only parsing errors
// We'd need a way of signalling that from the lexer itself?
trait LexToken { this: ErrorBuilder[_] =>

    def tokens: Seq[Parsley[String]]
    def whitespace: Parsley[_]

    private def makeParser: Parsley[((String, (Int, Int)), List[(String, (Int, Int))])] = {
        // we start at column 1 anyway
        //val trueCol = col.map(_ - 1)
        // cannot fail
        val tokWidth = lookAhead(manyUntil(item <~> Parsley.pos, whitespace <|> eof)).map {
            case csAndPs =>
                val (cs, ps) = csAndPs.unzip
                (cs.mkString, ps.last)
        }
        tokWidth <~> sequence(tokens.map(p => option(attempt(lookAhead(p <~> Parsley.pos)))): _*).map(_.flatten)
    }

    def selectToken(maxLine: Int, maxCol: Int, rawToken: String, matchedToks: List[(String, (Int, Int))]): Token = {
        val toks = matchedToks.sortBy(_._2).map {
            // FIXME: this is wrong, because the token's width might be 0 if it ate a newline!
            // For now, we can just guard against 0 carets
            case (name, (line, col)) => //Named(name, Width(Math.max(1, Math.min(width, maxWidth))))
                Named(name, UntilPos(Math.min(line, maxLine), if (line < maxLine) col else if (maxLine < line) maxCol else Math.min(col, maxCol)))
        }
        toks.lastOption.getOrElse(Raw(rawToken))
    }

    override def unexpectedToken(cs: Iterable[Char], amountOfInputParserWanted: Int): Token = {
        val Success(((rawToken, (rawLine, rawCol)), matchedToks)) = makeParser.parse {
            cs match {
                case cs: WrappedString => cs.toString
                case cs => cs.mkString
            }
        }
        // TODO: not really ideal, what about parsing comments etc?
        if (rawToken.isEmpty) Named("whitespace", Width(1))
        else selectToken(rawLine, rawCol, rawToken, matchedToks)
    }
}

object LexToken {
    def constantNames(ps: (Parsley[_], String)*): Seq[Parsley[String]] = ps.map {
        case (p, n) => p #> n
    }
}
// $COVERAGE-ON$

/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.errors.tokenextractors

import scala.collection.immutable.WrappedString

import parsley.Parsley, Parsley.{attempt, lookAhead}
import parsley.Success
import parsley.character.{item, newline, noneOf}
import parsley.combinator.{choice, eof, manyUntil, option, sequence, someUntil}
import parsley.errors.{helpers, ErrorBuilder, Named, Raw, Token, UntilPos}

// Turn coverage off, because the tests have their own error builder
// We might want to test this on its own though
// $COVERAGE-OFF$
trait LexToken { this: ErrorBuilder[_] =>

    def tokens: Seq[Parsley[String]]
    def whitespace: Parsley[_]

    // cannot fail
    private lazy val makeParser: Parsley[Either[String, List[(String, (Int, Int))]]] = {
        val toks = (whitespace #> "whitespace" +: tokens).map(p => attempt(p <~> Parsley.pos))
        val rawTok = lookAhead(someUntil(item, eof <|> choice(toks: _*))).map(_.mkString)
        rawTok <+> sequence(toks.map(p => option(lookAhead(p))): _*).map(_.flatten)
    }
    def selectToken(matchedToks: List[(String, (Int, Int))]): Token = {
        val toks = matchedToks.sortBy(_._2).map {
            case (name, (line, col)) => Named(name, UntilPos(line, col))
        }
        toks.last
    }

    override def unexpectedToken(cs: IndexedSeq[Char], amountOfInputParserWanted: Int, lexicalError: Boolean): Token = {
        if (!lexicalError) {
            // This is better, but does mean that the tokens provided should NOT consume terminal whitespace
            // One way of mitigating this would be to cap off the token to the next whitespace, but that's
            // a bit dodgy from the users perspective
            val Success(rawOrToks) = makeParser.parse {
                cs match {
                    case cs: WrappedString => cs.toString
                    case cs => cs.mkString
                }
            }
            rawOrToks.fold(Raw.apply, selectToken)
        }
        // No lexical extraction should occur here!
        else {
            // TODO: improve this!!!
            Raw(s"${cs.head}")
        }
    }
}

object LexToken {
    def constantNames(ps: (Parsley[_], String)*): Seq[Parsley[String]] = ps.map {
        case (p, n) => p #> n
    }
}
// $COVERAGE-ON$

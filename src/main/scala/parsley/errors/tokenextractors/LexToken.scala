/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.errors.tokenextractors

import scala.collection.immutable.WrappedString

import parsley.Parsley, Parsley.{attempt, lookAhead}
import parsley.Success
import parsley.character.{item, newline, noneOf}
import parsley.combinator.{choice, eof, manyUntil, option, sequence, someUntil}
import parsley.errors.{helpers, ErrorBuilder, Named, Raw, Token, UntilPos, Width}

// Turn coverage off, because the tests have their own error builder
// We might want to test this on its own though
// $COVERAGE-OFF$
// TODO: The problem with this is that it shouldn't apply the heuristics to lexing errors, only parsing errors
// We'd need a way of signalling that from the lexer itself?
trait LexToken { this: ErrorBuilder[_] =>

    def tokens: Seq[Parsley[String]]
    def whitespace: Parsley[_]

    // cannot fail
    /*private lazy val makeParser: Parsley[(Option[(String, (Int, Int))], List[(String, (Int, Int))])] = {
        val toks = tokens.map(p => attempt(lookAhead(p <~> Parsley.pos)))
        val rawTok = lookAhead(manyUntil(item <~> Parsley.pos, whitespace <|> eof)).map {
            case csAndPs =>
                val (cs, ps) = csAndPs.unzip
                ps.lastOption.map(p => (cs.mkString, p))
        }
        rawTok <~> sequence(toks.map(option): _*).map(_.flatten)
    }*/

    // cannot fail
    private lazy val makeParser2: Parsley[Either[String, List[(String, (Int, Int))]]] = {
        val toks = (whitespace #> "whitespace" +: tokens).map(p => attempt(p <~> Parsley.pos))
        val rawTok = lookAhead(someUntil(item, eof <|> choice(toks: _*))).map(_.mkString)
        rawTok <+> sequence(toks.map(p => option(lookAhead(p))): _*).map(_.flatten)
    }

    /*def selectToken(maxLine: Int, maxCol: Int, rawToken: String, matchedToks: List[(String, (Int, Int))]): Token = {
        val toks = matchedToks.sortBy(_._2).map {
            case (name, (line, col)) =>
                Named(name, UntilPos(Math.min(line, maxLine), if (line < maxLine) col else if (maxLine < line) maxCol else Math.min(col, maxCol)))
        }
        toks.lastOption.getOrElse(Raw(rawToken))
    }*/

    def selectToken(matchedToks: List[(String, (Int, Int))]): Token = {
        val toks = matchedToks.sortBy(_._2).map {
            case (name, (line, col)) => Named(name, UntilPos(line, col))
        }
        toks.last
    }

    override def unexpectedToken(cs: IndexedSeq[Char], amountOfInputParserWanted: Int, lexicalError: Boolean): Token = {
        /*val Success((rawTokOpt, matchedToks)) = makeParser.parse {
            cs match {
                case cs: WrappedString => cs.toString
                case cs => cs.mkString
            }
        }
        // not really ideal, what about parsing comments etc?
        rawTokOpt match {
            case None => Named("whitespace", Width(1))
            case Some((rawTok, (rawLine, rawCol))) => selectToken(rawLine, rawCol, rawTok, matchedToks)
        }*/
        // This is better, but does mean that the tokens provided should not consume terminal whitespace
        // One way of mitigating this would be to cap off the token to the next whitespace, but that's
        // a bit dodgy from the users perspective
        val Success(rawOrToks) = makeParser2.parse {
            cs match {
                case cs: WrappedString => cs.toString
                case cs => cs.mkString
            }
        }
        rawOrToks.fold(Raw, selectToken)
    }
}

object LexToken {
    def constantNames(ps: (Parsley[_], String)*): Seq[Parsley[String]] = ps.map {
        case (p, n) => p #> n
    }
}
// $COVERAGE-ON$

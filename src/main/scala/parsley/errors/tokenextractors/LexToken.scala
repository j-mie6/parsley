package parsley.errors.tokenextractors

import parsley.Parsley, Parsley.{attempt, col, lookAhead}
import parsley.Success
import parsley.character.{noneOf, newline}
import parsley.combinator.{eof, manyUntil, option, sequence}
import parsley.errors.{ErrorBuilder, helpers, Named, Raw, Token}

import scala.collection.immutable.WrappedString

trait LexToken { this: ErrorBuilder[_] =>

    def tokens: Seq[(Parsley[_], String)]
    def whitespace: Parsley[_]

    private def makeParser: Parsley[(String, List[(String, Int)])] = {
        // we start at column 1 anyway
        val trueCol = col.map(_ - 1)
        // cannot fail
        val tokWidth = manyUntil(noneOf('\n'), whitespace <|> eof <|> newline).map(_.mkString)
        tokWidth <~> sequence(tokens.map {
            case p -> name => option(attempt(lookAhead(p #> name <~> trueCol)))
        }: _*).map(_.flatten)
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
        selectToken(rawToken.length, rawToken, matchedToks)
    }
}

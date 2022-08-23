package parsley.errors.tokenextractors

import parsley.errors.{ErrorBuilder, helpers, Named, Raw, Token}

import scala.collection.immutable.WrappedString

trait MatchParserDemand { this: ErrorBuilder[_] =>
    override def unexpectedToken(cs: Iterable[Char], amountOfInputParserWanted: Int): Token = {
      cs match {
        case helpers.WhitespaceOrUnprintable(name) => Named(name, 1)
        // the default case will build a new string, if the underlying was already a string
        // this is redundant.
        case cs: WrappedString => Raw(cs.slice(0, amountOfInputParserWanted).toString)
        case _                 => Raw(cs.take(amountOfInputParserWanted).mkString)
      }
    }
}

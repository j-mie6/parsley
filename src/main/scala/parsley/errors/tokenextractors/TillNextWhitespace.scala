package parsley.errors.tokenextractors

import parsley.errors.{ErrorBuilder, helpers, Named, Raw, Token}

import scala.collection.immutable.WrappedString

trait TillNextWhitespace { this: ErrorBuilder[_] =>
    def trimToParserDemand: Boolean

    override def unexpectedToken(cs: Iterable[Char], amountOfInputParserWanted: Int): Token = {
      cs match {
        case helpers.WhitespaceOrUnprintable(name) => Named(name, 1)
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

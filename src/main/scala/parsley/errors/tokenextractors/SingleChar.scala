package parsley.errors.tokenextractors

import parsley.errors.{ErrorBuilder, helpers, Named, Raw, Token}

trait SingleChar { this: ErrorBuilder[_] =>
    override def unexpectedToken(cs: Iterable[Char], amountOfInputParserWanted: Int): Token = cs.head match {
        case helpers.WhitespaceOrUnprintable(name) => Named(name, 1)
        case c => Raw(s"$c")
    }
}

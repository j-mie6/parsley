package parsley.errors.tokenextractors

import parsley.errors.{ErrorBuilder, Token, Raw}

trait SingleChar { this: ErrorBuilder[_] =>
    override def unexpectedToken(cs: Iterable[Char], amountOfInputParserWanted: Int): Token = Raw(s"${cs.head}")
}

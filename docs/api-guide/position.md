{%
laika.title = "`parsley.position`"
parsley.tabname = "Position Combinators (parsley.position)"
laika.site.metadata.description = "This page describes how to get position information."
%}
# Position Combinators (`parsley.position`)

During a parse, position information is tracked and recorded by `parsley`.
The `parsley.position` module contains combinators that can access this
information so that it can be used in the construction of the results.

@:callout(info)
*The Scaladoc for this page can be found at [`parsley.position`](@:api(parsley.position$)).*
@:@

## Position Extraction
The `line` and `col` parsers will extract the line and column respectively. In
`parsley`, the first line and column are both 1. For convenience, the `pos`
parser is defined as `line.zip(col)`.

## Offset Extraction
The `offset` parser can be used to obtain the raw underlying offset of
a parser, which can be used for calculations on the number of characters (not codepoints) a parser consumes, but should not be used to derive positions themselves.

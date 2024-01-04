{%
laika.versioned = true
laika.title = "`parsley.combinator`"
parsley.tabname = "Additional Combinators (parsley.combinator)"
laika.site.metadata.description = "This page describes additional helpful combinators."
%}
# Additional Combinators (`parsley.combinator`)
While many combinators are implemented as methods directly on the `Parsley` type,
some are left as functions within the `combinator` module. These mostly involve
handling repeating parsers. Not all the combinators are discussed here.

@:callout(info)
*The Scaladoc for this page can be found at [`parsley.combinator`](@:api(parsley.combinator$)).*
@:@

## Iterative Combinators
One of the main classes of combinator are the iterative combinators, which
execute parsers multiple times until they cannot match any more; the results
of these combinators vary. If the parser being repeated fails having consumed
input, iterative combinators will fail; if no input was consumed on failure,
the iteration will stop.

The most commonly used of these are the `many` and `some` combinators, which
return a list of the successful results:

```scala mdoc:to-string
import parsley.character.digit
import parsley.combinator.{many, some}

many(digit.zip(digit)).parse("")
many(digit.zip(digit)).parse("1234")
many(digit.zip(digit)).parse("12345")

some(digit.zip(digit)).parse("")
some(digit.zip(digit)).parse("1234")
some(digit.zip(digit)).parse("12345")
```

While `some` will parse one or more times, `manyN` generalises to work for
any minimum required parses `n`. When the results are not needed, `skipMany`,
`skipSome`, and `skipManyN` can be used instead. To determine how many times
the parse was successful, the `count` and `count1` can be used instead.

The `manyUntil` and `someUntil` combinators can be used to parse iteratively
until some other parse is successful: this can be used, for instance, to
scan comments:

```scala mdoc:to-string
import parsley.character.{string, item, endOfLine}
import parsley.combinator.{manyUntil}

val comment = string("//") ~> manyUntil(item, endOfLine)
comment.parse("// this is a comment\n")
```

### Separators
There are three variants of the iterative combinators that handle delimited
parsing: reading something separated (or ended) by another delimiter:

* `sepBy1`: parses input like `x, y, z`
* `endBy1`: parses input like `x; y; z;`
* `sepEndBy1`: parses input like `x, y, z` *or* `x, y, z,`

## Optional Combinators
The `option`, `optional`, and `optionalAs` combinators can be used to
optionally parse something. The `option` combinator will inject the
result into an `Option`; the `optional` combinator just returns `Unit`;
and the `optionalAs` combinator unconditionally returns any given value.

```scala mdoc:to-string
import parsley.character.digit
import parsley.combinator.option

option(digit.zip(digit)).parse("")
option(digit.zip(digit)).parse("0")
option(digit.zip(digit)).parse("09")
```

## Conditional Combinators
The conditional combinators are used for conditional execution
of a parser. These include `ifP`, `guard`, `when`, and `whileP`.
As may be expected, `ifP` models an *if-else-expression*, `whileP` models
a *while-loop*, and `when` models an *if-statement*. They mostly
find their utility when used in conjunction with [registers](state.md).

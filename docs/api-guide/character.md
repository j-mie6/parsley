{%
laika.title = "`parsley.character`"
parsley.tabname = "Parsing Characters (parsley.character)"
laika.site.metadata.description = "This page describes how to read characters."
%}

# Parsing Characters
The consumption of input is central to parsing. In `parsley`, the only way of
consuming input is by consuming characters, as customised token streams are
not supported. However, as `parsley` is based on Scala, which relies on
16-bit Basic-Multilingual Plane UTF16 characters, there are two modules for
input consumption: `character`, for 16-bit `Char`, and `unicode`, handling 32-bit `Int`s.

@:callout(info)
*The Scaladoc for this page can be found at [`parsley.character`](@:api(parsley.character$)) and [`parsley.unicode`](@:api(parsley.unicode$)).*
@:@

## Character Primitives
There are three key input consumption primitives in `parsley`: `satisfy`, which
takes a predicate and parses any character for which that predicate returns true; `char`, which reads a specific character; and `string`, which reads a specific
sequence of characters. Technically, `char` and `string` can be built in terms of
`satisfy`, but would then lose their enhanced error message characteristics.

The `oneOf` and `noneOf` are based on `satisfy` but with better error messages,
based on the kind of range of characters they are provided with.
The `strings` and `trie` combinators can be used similarly to match a set of
strings: they are likely to parse these more efficiently than a manual
construction and may improve further in future.

## String Building
It is possible to efficiently construct a `String` using the `stringOfMany` and
`stringOfSome` combinators. This can be used with either a character predicate (which turns out to be called `takeWhileP` in `megaparsec`), or can be given a
parser that consumes a character instead. The construction of the underlying
string is done efficiently with a `StringBuilder`. Note that, with the version
that takes a parser, the characters incorporated into the string are the ones
returned by the parser and not necessarily the ones the parser consumed.

## Pre-Built Character Parsers
Some pre-built parsers are bunched into `character` and `unicode`, which parse
specific sets of letter, with good error messages. Each will document the specific
set of characters they recognise.

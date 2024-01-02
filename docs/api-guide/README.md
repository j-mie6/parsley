{%
helium.site.pageNavigation.enabled = false
laika.site.metadata.description = "This page outlines the API guide."
%}

# Understanding the API

@:todo(TODO: I want a discussion here about lazy and strict positions, that's a very important thing to discuss!)

## Main Classes and Packages
In `parsley`, everything resides within the @:api(parsley) package, and the major entry point is
[`parsley.Parsley`](@:api(parsley.Parsley)).
There are a few modules of note:

* [`parsley.Parsley`](Parsley.md):
  contains some of the basic and primitive combinators (at least those that aren't methods on
  parsers).
* [`parsley.combinator`](@:api(parsley.combinator$)):
  contains handy combinators, this should be your first port of call
  when you want to do something but are not sure a combinator exists for it. At the very
  least, the `eof` combinator is very common.
* [`parsley.character`](@:api(parsley.character$)):
  contains a variety of combinators which deal with characters, key ones include `char`,
  `satisfy` and `string`.
* [`parsley.implicits`](@:api(parsley.implicits$)):
  contains the very useful implicit conversion combinators. In particular, importing `charLift`
  and `stringLift` allows you write character and string literals as if they were parsers
  themselves. There are also implicit classes here which extend functions of any arity with a
  corresponding `.lift` method, instead of using the `liftN` functions.
* [`parsley.expr`](@:api(parsley.expr$)):
  contains the machinery needed to generate expression parsers for you based, at its simplest, on
  a table of operators in order of precedence. This is _well_ worth a look (this is covered in
  detail in [Building Expression Parsers].
* [`parsley.token`](@:api(parsley.token$)):
  contains a bunch of functionality for performing common lexing tasks, which is _very_ configurable.
  These parsers may also be optimised for performance.

### Using `parsley.token` for Lexing
Unlike Haskell libraries like `megaparsec` and `parsec`, `parsley` does not
tie the lexing functionality to the Haskell Report, instead supporting a superset of the functionality. The functionality is provided to the user by @:api(parsley.token.Lexer), and this must be provided an value of type @:api(parsley.token.descriptions.LexicalDesc), which provides all the configuration necessary to describe the language and make the parsers.

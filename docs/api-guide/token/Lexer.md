{%
laika.title = "`Lexer`"
%}

# Lexer (`parsley.token.Lexer`)

The `Lexer` class is the main-entry point to the combinator-based functionality of the `parsley.token`
package. It is given configuration in the form of a [`LexicalDesc`][`descriptions.LexicalDesc`]
and an optional [`ErrorConfig`][`errors.ErrorConfig`]. The internal structure is then a collection
of objects that contain various forms of functionality: these are explored in more detail in this
page.

It is worth noting the highest-level structure:

* `lexeme` and `nonlexeme` are the top level categorisation of functionality, accounting for
  whitespace
* `fully` is a combinator designed to be used around the **outer-most** parser, ran **at most once**
   during a parse, to consume leading whitespace and ensure all input is consumed.
* `space` is an object that allows for explicit interaction with whitespace parsing: this is
  really only important for whitespace-sensitive languages, and `lexeme` should be used for
  almost all other applications.

@:callout(info)
*The Scaladoc for this page can be found at [`parsley.token.Lexer`][@:api(parsley.token.Lexer)].*
@:@

## Distinguishing Between "Lexeme" and "Non-Lexeme"
Broadly, the `Lexer` duplicates the vast majority of its functionality between two different
objects: `lexeme` and `nonlexeme`. Broadly speaking, everything within `nonlexeme` can be
found inside `lexeme`, but not the other way around. The name "lexeme" is not an amazing one
terminology wise, but there is a historical precedence set by `parsec`.

@:style(paragraph) Non-lexeme things @:@ @:todo(TODO: LexToken referenced here please!)
A non-lexeme thing does not care about whitespace: these are raw tokens. It is highly likely that
you wouldn't want to use these in a regular parser, but they may be handy for **custom error handling**
or **building composite tokens**.

@:style(paragraph) Lexeme things @:@
These do account for whitespace that occurs *after* a token, consuming everything up until the
next token. This means there are some extra pieces of functionality available that don't make much
sense for non-lexeme handling. The `lexeme` object can also be used as a function
via its `apply` method, allowing it to make any parser into one that handles whitespace: this should
be done for any composite tokens made with `nonlexeme`.

@:callout(warning)
Whitespace handling should ideally be handled *uniformly* by `lexeme`: it establishes a convention
of only consuming **trailing** whitespace, which is **important** for avoiding ambiguity in a parser.
If you cannot use `lexeme.apply`, you *must* adhere to this same convention.

For handling initial whitespace in the parser (before the very first token), you should use `Lexer.fully`.
@:@

## `Lexer.{lexeme, nonlexeme}.numeric`

## `Lexer.{lexeme, nonlexeme}.text`

## `Lexer.{lexeme, nonlexeme}.symbol`

## `Lexer.{lexeme, nonlexeme}.names`

## `Lexer.lexeme.{enclosing, separators}`

## Whitespace-Sensitive Languages and `Lexer.space`

@:callout(error)
If the `LexicalDesc.spaceDesc.whitespaceIsContextDependent` flag is turned on it is **crucial** that
either the `Lexer.fully` combinator is used, *or* `Lexer.space.init` is ran as the very first thing the
top-level parser does. Without this, the context-dependent whitespace will not be set-up correctly!
@:@

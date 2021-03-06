This is the documentation for Parsley.

== Package structure ==

The [[parsley]] package contains the [[parsley.Parsley `Parsley`]] class, as well as
the [[parsley.Result `Result`]], [[parsley.Success `Success`]], and [[parsley.Failure `Failure`]]
types. In addition to these, it also contains the following packages and "modules" (a module
is defined as being an object which mocks a package):

  - [[parsley.Parsley$ `parsley.Parsley`]] contains the bulk of the core "function-style" combinators,
    as well as the implicit classes which enable the "method-style" combinators.
  - [[parsley.combinator$ `parsley.combinator`]] contains many helpful combinators that simplify some
    common parser patterns.
  - [[parsley.character$ `parsley.character`]] contains the combinators needed to read characters and
    strings, as well as combinators to match specific sub-sets of characters.
  - [[parsley.debug$ `parsley.debug`]] contains debugging combinators, helpful for identifying faults
    in parsers.
  - [[parsley.io$ `parsley.io`]] contains extension methods to run parsers with input sourced from
    IO sources.
  - [[parsley.expr `parsley.expr`]] contains the following sub modules:
    - [[parsley.expr.chain `parsley.expr.chain`]] contains combinators used in expression parsing
    - [[parsley.expr.precedence `parsley.expr.precedence`]] is a builder for expression parsers built
      on a precedence table.
  - [[parsley.implicits$ `parsley.implicits`]] contains several implicits to add syntactic sugar
    to the combinators, such as being able to use character and string literals directly as parsers,
    as well as enabling lifting of functions to work on parsers.
  - [[parsley.lift$ `parsley.lift`]] contains functions which lift functions that work on regular
    types to those which now combine the results of parsers returning those same types. these are
    ubiquitous.
  - [[parsley.registers$ `parsley.registers`]] contains combinators that interact with the
    context-sensitive functionality in the form of registers.
  - [[parsley.token `parsley.token`]] contains the [[parsley.token.Lexer `Lexer`]] class that provides
    a host of helpful lexing combinators when provided with the description of a language.
  - [[parsley.unsafe$ `parsley.unsafe`]] contains unsafe (and not thread-safe) ways of speeding up
    the execution of a parser.
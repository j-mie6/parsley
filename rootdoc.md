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

In addition to the modules and packages outlined above, this version of Parsley (up to version 3.0),
also includes the so-called `old-style` API, which is deprecated (see
[[https://github.com/j-mie6/Parsley/wiki/The-Newstyle-API-vs-the-Oldstyle-API the Parsley wiki]] for
a discussion of these differences). You should use the modules described above, and '''avoid''' the following:

  - `parsley.BitGen`
  - `parsley.Char`
  - `parsley.CharSet`
  - `parsley.Combinator`
  - `parsley.ExpressionParser`
  - `parsley.Impl`
  - `parsley.Implicits`
  - `parsley.LanguageDef`
  - `parsley.NotRequired`
  - `parsley.Parser`
  - `parsley.Predicate`
  - `parsley.Reg`
  - `parsley.TokenParser`
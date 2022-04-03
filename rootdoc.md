This is the documentation for Parsley.

== Package structure ==

The [[parsley]] package contains the [[parsley.Parsley `Parsley`]] class, as well as
the [[parsley.Result `Result`]], [[parsley.Success `Success`]], and [[parsley.Failure `Failure`]]
types. In addition to these, it also contains the following packages and "modules" (a module
is defined as being an object which mocks a package):

  - [[parsley.Parsley$ `parsley.Parsley`]] contains the bulk of the core "function-style" combinators.
  - [[parsley.combinator$ `parsley.combinator`]] contains many helpful combinators that simplify some
    common parser patterns.
  - [[parsley.character$ `parsley.character`]] contains the combinators needed to read characters and
    strings, as well as combinators to match specific sub-sets of characters.
  - [[parsley.debug$ `parsley.debug`]] contains debugging combinators, helpful for identifying faults
    in parsers.
  - [[parsley.extension$ `parsley.extension`]] contains syntactic sugar combinators exposed as
    implicit classes.
  - [[parsley.io$ `parsley.io`]] contains extension methods to run parsers with input sourced from
    IO sources.
  - [[parsley.expr `parsley.expr`]] contains the following sub modules:
    - [[parsley.expr.chain$ `parsley.expr.chain`]] contains combinators used in expression parsing
    - [[parsley.expr.precedence$ `parsley.expr.precedence`]] is a builder for expression parsers built
      on a precedence table.
    - [[parsley.expr.infix$ `parsley.expr.infix`]] contains combinators used in expression parsing,
      but with more permissive types than their equivalents in `chain`.
    - [[parsley.expr.mixed$ `parsley.expr.mixed`]] contains combinators that can be used for
      expression parsing, but where different fixities may be mixed on the same level: this is rare
      in practice.
  - [[parsley.implicits `parsley.implicits`]] contains several implicits to add syntactic sugar
    to the combinators. These are sub-categorised into the following sub modules:
     - [[parsley.implicits.character$ `parsley.implicits.character`]] contains implicits to allow you
       to use character and string literals as parsers.
     - [[parsley.implicits.combinator$ `parsley.implicits.combinator`]] contains implicits related to
       combinators, such as the ability to make any parser into a `Parsley[Unit]` automatically.
     - [[parsley.implicits.lift$ `parsley.implicits.lift`]] enables postfix application of the lift
       combinator onto a function (or value).
     - [[parsley.implicits.zipped$ `parsley.implicits.zipped`]] enables boths a reversed form of
       lift where the function appears on the right and is applied on a tuple (useful when type
       inference has failed) as well as a `.zipped` method for building tuples out of several
       combinators.
  - [[parsley.errors `parsley.errors`]] contains modules to deal with error messages, their refinement
    and generation.
     - [[parsley.errors.combinator$ `parsley.errors.combinator`]] provides combinators that can be
       used to either produce more detailed errors as well as refine existing errors.
  - [[parsley.lift$ `parsley.lift`]] contains functions which lift functions that work on regular
    types to those which now combine the results of parsers returning those same types. these are
    ubiquitous.
  - [[parsley.registers$ `parsley.registers`]] contains combinators that interact with the
    context-sensitive functionality in the form of registers.
  - [[parsley.token `parsley.token`]] contains the [[parsley.token.Lexer `Lexer`]] class that provides
    a host of helpful lexing combinators when provided with the description of a language.
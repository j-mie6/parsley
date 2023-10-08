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
*The Scaladoc for this page can be found at [`parsley.token.Lexer`](@:api(parsley.token.Lexer)).*
@:@

## Distinguishing Between "Lexeme" and "Non-Lexeme"
Broadly, the `Lexer` duplicates the vast majority of its functionality between two different
objects: `lexeme` and `nonlexeme`. Broadly speaking, everything within `nonlexeme` can be
found inside `lexeme`, but not the other way around. The name "lexeme" is not an amazing one
terminology wise, but there is a historical precedent set by `parsec`.

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

## `Lexer.{lexeme, nonlexeme}.names`

## `Lexer.{lexeme, nonlexeme}.symbol`

## `Lexer.{lexeme, nonlexeme}.numeric`
This object contains the definitions of several different parsers for handling *numeric* data:
this includes both integers and floating point numbers. The configuration for all of these parsers
is managed by [`LexicalDesc.numericDesc`](@:api(parsley.token.descriptions.numeric.NumericDesc)).
The members of the `numeric` object a split into three kinds:

* [`parsley.token.numeric.Integer`](@:api(parsley.token.numeric.Integer)): values with this type
  deal with whole numbers, and this interface in particular has support for different bases of
  number as well as various bit-widths of parsed data. When the bit-width of the parser is restricted,
  the generated result can be any numeric type that is wide enough to accommodate those values. If
  the parsed int does not fit in the required bounds, a parse error will be generated. If no bit-width
  is specified, an arbitrary `BigInt` is used.

  The supported bit-widths within `parsley` are 8, 16, 32, and 64. When one of these widths is chosen,
  the Scala compiler will automatically pick a result type that matches the same width (so `Int` for 32).
  If the generic parameter is filled manually, the given type will be used instead as long as it is
  wide enough.

  Currently, there is no way of adding new bit-widths or defining custom numeric container types.

* [`parsley.token.numeric.Real`](@:api(parsley.token.numeric.Real)): values with this type
  deal with floating-point numbers **only**: values without a point or an exponent (if allowed)
  will not be parsed by these parsers. Like `Integer`, different bases can be specified: in this
  case the meaning of exponents can be controlled within the configuration, for instance, a
  hexadecimal floating-point literal like `0xAp4` classically would represent `10 * 2^4`, or `160`,
  because `p` represents an exponent delimiter and hexadecimal exponents are normally base 2 (but
  this is fully configurable in `parsley`).

  Compared to `Integer`, different precisions can be chosen for `Real`, allowing for
  arbitrary-precision floats, `Float`, and `Double` results. For the stricter representations,
  there is a `doubleRounded`/`floatRounded` that just gives the nearest valid value (with no parse
  errors), and a `double`/`float` which demands that the parsed literal is *precisely* representable
  or else gives a parse error.

* [`parsley.token.numeric.Combined`](@:api(parsley.token.numeric.Combined)): values with this type
  can deal with both integers and floating-point numbers. This is done by returning one or the other
  as part of an `Either`. A *slightly* limited selection of bit-widths and precisions are available
  for both parts. The draw of these combinators is that they may remove the ambiguity between the
  two kinds of literal so that no backtracking is required within the parser.

@:callout(info)
The configuration which specifies which of the numeric bases are legal for a number literal applies
only to the `number` parsers within `Integer`, `Real`, and `Combined`. A parser for a specific base
can always just be used directly, even when otherwise disabled in configuration.
@:@

### Examples of Configuration and Valid Literals
The [`plain`](@:api(parsley.token.descriptions.numeric.NumericDesc$#plain))
definition of `NumericDesc` provides a variety of different configurations for the
numeric literals depending on the literal base, so it mostly suffices to look at the effects of
these on the different bases to get a sense of what does what.

The basic configuration allows `number` to work with hexadecimal and octal literals, as well
as decimal. These have their standard prefixes of `0x` and `0o`, respectively (or uppercase
variants). This means that `unsigned.number` will allow literals like `0`, `0xff`, `0o45`
and `345`. Each of these may be preceded by a `+` sign, but this is not *required*; if
`positiveSign` is set to `PlusSignPresence.Compulsory`, positive literals would always
require a `+`; and if it is set to `PlusSignPrecense.Illegal`, the `+` prefix can never be
used (but `-` is fine for `signed` parsers). By default, `023` is legal, but this can be
disabled by setting `leadingZerosAllowed` to `false`.

In the basic configuration, break characters are not supported. However, by setting
`literalBreakChar` to `BreakCharDesc.Supported('_', allowedAfterNonDecimalPrefix = true)`,
say, will allow for `1_000` or `0x_400`. Setting the second parameter to `false` will forbid
the latter example, as the break characters may then only appear between digits.

Real numbers in the default configuration do not support literals like `.0` or `1.`, this
behaviour must be explicitly enabled with `trailingDotAllowed` and `leadingDotAllowed`: note
that `.` is not a valid literal, even with both flags enabled! By default, all four different
bases support exponents on their literals for floating-point numbers. This could be turned
off for each by using `ExponentDesc.NoExponents`. However, with exponents enabled, it is
configured that the non-decimal bases all *require* exponents for valid literals. Whilst
`3.142` is valid decimal literal, `0x3.142` is not a legal hexadecimal literal: to make
it work, the exponent must be added, i.e. `0x3.142p0`, where `p0` is performing `* 2^0`.
For each of the non-decimal literals, the base of the exponent is configured to be `2`, hence
`2^0` in the previous example; for decimal it is set to the usual `10`, so that `2e3` is `2*10^3`, or `2000`. Notice that literals do not *require* a point, so long as they do have
an exponent.

@:callout(info)
When a floating point literal is parsed in a non-decimal base, the meaning of each digit past
the point is to be a fraction of that base. The example `0x3.142p0`, for instance is not equal
to the decimal `3.142`. Instead, it is equal to `(3 + 1/16 + 4/(16^2) + 2/(16^3)) * 2^0 = 3.07861328125`. Handily, hexadecimal floats are still equal to the 4-bit bunching up of binary
floats: `0x0.Bp0` is the same as `0b0.1011p0`, both of which are `0.6875` in decimal.
@:@

## `Lexer.{lexeme, nonlexeme}.text`

## `Lexer.lexeme.{enclosing, separators}`

## Whitespace-Sensitive Languages and `Lexer.space`

@:callout(error)
If the `LexicalDesc.spaceDesc.whitespaceIsContextDependent` flag is turned on it is **crucial** that
either the `Lexer.fully` combinator is used, *or* `Lexer.space.init` is ran as the very first thing the
top-level parser does. Without this, the context-dependent whitespace will not be set-up correctly!
@:@

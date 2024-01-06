{%
laika.versioned = true
laika.title = "`parsley.Parsley`"
parsley.tabname = "Parsley"
laika.site.metadata.description = "This page describes the main API of Parsley."
%}
# Parsley (`parsley.Parsley`)

All parsers have type `Parsley`, which has many methods (combinators) for composing parsers
together. The companion object also contains some primitive combinators and parsers.

@:callout(info)
*The Scaladoc for this page can be found at [`parsley.Parsley` (class)](@:api(parsley.Parsley))
and [`parsley.Parsley` (object)](@:api(parsley.Parsley$)).*
@:@

## Class `Parsley`
The `Parsley` class is the value class responsible for representing parsers.
It has methods largely grouped into the following categories:

* Methods to execute a parser, like `parse`.
* Methods to alter the result of a parser, like `map`, `as`, `void`, and `span`.
* Methods to sequence multiple parsers, like `flatMap`, `zip`, `<~`, and `~>`.
* Methods to compose parsers in alternation, like `orElse`, `|`, and `<+>`.
* Methods to filter the results of parsers, like `filter`, `collect`,
  and `mapFilter`.
* Methods to repeat a parser and collapse the results, like `foldLeft`,
  `reduceRight`, and so on.
* Special methods to help direct `parsley`, like `impure`, `overflows`, and
  `force`.

### Running Parsers
The `parse` method runs a parser with some given input. The type signature has
an additional `Err` parameter, with an implicit requirement for
`ErrorBuilder[Err]`. This is discussed in
[`parsley.errors.ErrorBuilder`][Constructing Custom Errors], but for
the purposes of basic use, you can assume that Scala will automatically infer
this type to be `String`, for which an `ErrorBuilder[String]` does implicitly
exist.

By importing `parsley.io._`, another `parseFromFile` method is added to
`Parsley`, which works similarly, but loads the input from a file first.
In future versions of `parsley`, this import will no longer be needed, and
this will be an overloading of `parse`.

### Altering Results
The results of an individual parser can be altered using `map` or combinators
derived from it, such as `as` or `void`. Note that the `#>` combinator is
a symbolic alias for `as`, though as is now recommended.

@:callout(info)
Note that, whilst `as` can be implemented using `map`, it is
actually implemented with `~>` and `pure`, which allows it to be better
optimised: if a result is not needed, `parsley` will ensure it is not
even generated to begin with.
@:@

The `span` combinator is special: it allows for the result of a parser to be discarded and instead the input consumed in the process of parsing is returned
instead. This can be used to avoid constructing strings again after parsing.

### Composing Parsers
Some combinators allow for multiple parsers to be composed together to form
a new one. This comes in two forms: sequencing them one after another and
combining their results in some way, or combining them in parallel as two
independent choices.

#### Sequencing
The `zip` combinator is the most permissive way of combining two parsers together
whilst retaining both of their results; `p zip q` will parse `p`, then `q` and
then return a pair of their results. The symbolic version of `zip` is called `<~>`. When both results are not needed, `~>` and `<~` point towards a result to
keep, and discard the other.

Combinators like `<*>`, `<**>` and `<::>` perform a similar role to `zip`, but
combine the two results depending on the specific subtypes of the arguments.
Both `<*>` and `<**>` perform function application, and `<::>` adds the result
of the first parser onto a list returned by the second. These are all more
specific versions of the `parsley.lift._` combinators, but are often useful
in practice.

The `flatMap` combinator is another way of sequencing two parsers, but where
the second depends on the result of the first. In `parsley`, this operation
is **very** expensive, and it (and its derived `flatten` combinator) should
be avoided. One way of avoiding the `flatMap` combinator is to use features
found in [`parsley.state`][Context-Sensitive Parsing (`parsley.state`)],
or to use `lift` combinators instead.

#### Choice
When one of two parsers can be used at a specific point, the `|` combinator (also
known as `orElse` or `<|>`) can be used to try one and then the other if the
first failed. The result of the successful branch, if any, will be returned.

@:callout(warning)
A parser `p | q` will **only** try `q` if the parser `p` failed having **not**
consumed any input. If it does consume input, then the overall parser will fail.
The `atomic` combinator can be used to prevent input consumption on failure,
however.

When `atomic` is used in this manner, it may cause error messages to be less
effective, or increase the runtime or complexity of the parser. Backtracking
in this way should be avoided if at all possible by factoring the grammar or
employing techniques like [*Disambiguator Bridges*][Normalising or Disambiguating Data].
@:@

### Filtering Results
When the results of the parser need to be verified or conditionally transformed,
filtering combinators can be used. Largely, they will try and apply a function
to the result of a parser: if the function returns `false` or is otherwise not defined, then the parser will fail, and otherwise, it may perform some
transformation:

* `filter` will check if the result of a parser matches some predicate, failing
  otherwise. If successful, result is returned unchanged
* `collect` will attempt to apply a partial function to the result of the parser:
  if the function is defined on that input, it is applied and the new result
  returned; otherwise, the parser fails.
* `filterMap` is similar to `collect`, but works with a function that returns
  `Option[B]`: if the function returns `None`, the parser fails; otherwise the
  value inside the `Some` is returned.

Largely, these combinators are more useful in their more advanced formulations,
those in `parsley.errors.combinator.ErrorMethods`, which produce detailed
error messages about filtering failures.

### Reductions
There are a collection of methods that repeatedly perform a parser, and collapse
the generated results into a single value - like regular folds. There are three classes of reductive combinators:

* `foldLeft`, `foldRight`: these combine up the results of repeated parsing
  of a parser starting with an initial value and combining each result with
  the running value. When the name of the combinator has a `1` at the end, it
  means the parser must succeed at least once; otherwise the parser need not
  succeed and the combinator will return the initial value (so long as no input
  was consumed in the process).
* `reduceLeft`, `reduceRight`: these combine one or more parses of a parser
  by a left or right reduction. As the parser will parse successfully at least
  once, there is no need for a default or initial value.
* `reduceLeftOption`, `reduceRightOption`: like `reduceLeft` or `reduceRight`,
  but returns `None` if the parser could not successfully parse once, and wraps
  the result of reduction in a `Some` otherwise.

### Special Methods
The underlying implementation of `parsley` is more akin to a compiler than
a regular parser combinator library. Some methods exist to help direct the
internal compiler:

* `force()`: this forces the compiler to optimise and compile the parser
  right now, as opposed to when the parser is first executed. This should be
  used on the top-level parser.
* `impure`: this tells the optimiser to not touch the parser, which may
  be necessary when the parser deals with mutable objects. This is because
  the optimiser assumes purity, and may factor out or even remove results
  entirely, which would otherwise change the results. This can be used
  across the parser.
* `overflows()`: this tells the compiler that the parser it is invoked on
  will stack overflow during compilation. As a result, the compiler will
  instead process the parser in a stack-safe trampoline. This should be
  used on the top-level parser.

## Object `Parsley`
The `Parsley` object contains a collection of primitive combinators that help
control a parser or its results. By far the most important ones are `pure` and
`atomic`:

* `pure` injects a value into the parsing world without having any other effect
  on the state of the parser.
* `atomic` ensures that a parser either consumes all its input and succeeds, or
  none of it and fails. This is important because the `|` combinator will not
  parse its second argument if the first failed having consumed input - `atomic`
  helps by ensuring no input was consumed on failure.

In addition to these, it also has `empty` and `empty(Int)`, which fail the parser
immediately (but recoverably) and produces a given width of caret (and `0` in the case of `empty`); `lookAhead` and `notFollowedBy` for dealing with positive
and negative lookahead; and `fresh`, which can be used like `pure` but evaluates
its argument every time, which is useful in the presence of mutable values.

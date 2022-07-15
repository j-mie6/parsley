# Parsley Internals (`parsley.internal`)

> _Here be Dragons!_

Parsley is a deep-embedded parser combinator library. This means that the
combinators themselves are just syntax and perform no task on their own.
To actually parse with this so-called _combinator tree_, it first must be
processed: all manipulations and processing on the combinator tree (along with
the tree's definition) can be found in `parsley.internal.deepembedding`. Further
description of what goes on in that package can be found within.

The final output of this processing state is an `Array[Instr]`, where `Instr`
is a datatype that represents Parsley's abstract instruction set. It is these
instructions that are executed to actually do parsing on some input. The
machinery required to actually do parsing can be found within
`parsley.internal.machine`. Further description of what goes on in that package
can be found within.

Parsley supports custom error types via the `ErrorBuilder` mechanism in the
`parsley.errors` package. However, internally, Parsley uses its own error message
type called `ParseError`, which can be formatted using the `ErrorBuilder`. The
relevant datastructures that make up the internal description of error messages
can be found in `parsley.internal.errors`.

Parsley makes use of a few custom purpose-built datastructures, these are used
for a variety of tasks throughout the library and the most general structures are
kept in `parsley.internal.collection.mutable/immutable`.

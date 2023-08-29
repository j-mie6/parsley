```scala mdoc:invisible
import parsley.Parsley, Parsley._
```

# Generic Bridges

The *Parser Bridge* pattern is a technique for decoupling semantic actions from the parser itself.
The [`parsley.genericbridges`][@:api(parsley.genericbridges$)] module contains 23 classes that allow
you to get started using the technique straight away if you wish.

## What are *Parser Bridges*?
Without making use of *Parser Bridges*, results of parsers are usually combined by using `lift`,
`map`, or `zipped`:

```scala mdoc:invisible
val px: Parsley[Int] = pure(5)
val py: Parsley[Int] = pure(6)
```

```scala mdoc
import parsley.implicits.zipped.Zipped2
case class Foo(x: Int, y: Int)
// with px, py of type Parsley[Int]
val p = (px, py).zipped(Foo(_, _))
```

These work fine for the most part, however, there are couple of problems with this:

1. In Scala 3, `Foo(_, _)` actually needs to be written as `Foo.apply`, which introduces some (minor) noise;
   `zipped` itself is even contributing noise.
1. `Foo` itself is a simple constructor, if it gets more complex, readability rapidly decreases:
    - The result produced may require inspection of the data, including pattern matching (see [Normalising or Disambiguating Data]).
    - Additional information may need to be threaded in, like position information.
    - Data invariances may need to be enforced (see [Enforcing Invariances]).
1. Constructor application is on the right of the data, which people may find harder to read;
   this can be mitigated with `lift`, but that may run into type inference issues.

For some people (1) or (3) may not be an issue, or can be tolerated, but (2) can get out of hand
quickly. For larger parsers, properly decoupling these issues can make a huge difference to the
maintainability.

@:style(paragraph) How do bridges help? @:@
In short, a bridge is an object that provides an `apply` method that takes parsers as arguments,
as opposed to values. This means that they can be used directly in the parser with the logic kept
elsewhere. While you can just define a bridge manually with an `apply` method (or even just as a
function), it is more ergonomic to *synthesise* a bridge in terms of a function that does not interact
with `Parsley` values. If we assume that the companion object of `Foo` has been turned into such a
bridge (definition below), the above example can be written as:

```scala mdoc
val q = Foo(px, py)
```

In this version, the act of constructing the `Foo` value has been abstracted behind the bridge, `Foo`:
this means that the underlying implementation can vary without changing the parser.

@:style(paragraph) What are *Generic Bridges*? @:@
Generic bridges are the templating mechanism that allow for the synthesis of an `apply` method that
works on values of type `Parsley` from another that does not. While you can define your own bridge
templates (see [the associated tutorial](../tutorial/parser-bridge-pattern.md) for an explaination),
`parsley` provides some basic ones to get you started.

## How to use

```scala mdoc
import parsley.genericbridges.ParserBridge2
object Foo extends ParserBridge2[Int, Int, Foo]
```

## Use Cases

### Normalising or Disambiguating Data

### Enforcing Invariances

@:todo(Talk about the error messages when that's added in 5.0?)

## When *not* to use

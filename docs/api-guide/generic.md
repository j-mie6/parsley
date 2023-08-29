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
templates (see [the associated tutorial](../tutorial/parser-bridge-pattern.md) for an explanation),
`parsley` provides some basic ones to get you started.

## How to use
The [`parsley.genericbridges`][@:api(parsley.genericbridges$)] module contains `ParserBridge1` through
`ParserBridge22` as well as `ParserBridge0`; they all extend `ParserBridgeSingleton`, which provides
some additional combinators.

### `ParserBridge1[-A, +B]` through `ParserBridge22[-A, .., -V, +W]`
Each of these traits are designed to be implemented ideally by a companion object for a `case class`.
For example, the `Foo` class above can have its companion object turned into a bridge by extending
`ParserBridge2` (which is for two argument bridges):

```scala mdoc
import parsley.genericbridges.ParserBridge2
object Foo extends ParserBridge2[Int, Int, Foo]
```

This defines `def apply(px: Parsley[Int], py: Parsley[Int]): Parsley[Foo]`, implementing it in
terms of `def apply(x: Int, y: Int): Foo`, which is included as part of Scala's automatic `case class`
implementation. By making use of a companion object, this is *all* the boilerplate required to
start using the bridge. Of course, it's possible to define standalone bridges as well, so long as
you provide an implementation of `apply`, as illustrated by this error:

```scala mdoc:fail
object Add extends ParserBridge2[Int, Int, Int]
```

```scala mdoc:invisible
object Add extends ParserBridge2[Int, Int, Int] {
    def apply(x: Int, y: Int) = x + y
}
```


Implement that `apply` method and it's good to go! Of course, if the traits are mixed into a regular `class`,
they can also be parametric:

```scala mdoc:fail
class Cons[A] extends ParserBridge2[A, List[A], List[A]]
```

### `ParserSingletonBridge[+T]`
All the generic bridges extend the `ParserSingletonBridge` trait instantiated to a function type. For
example, `trait ParserBridge2[-A, -B, +C] extends ParserSingletonBridge[(A, B) => C]`. This means that
every bridge uniformly gets access to a couple of extra combinators in addition to their `apply`:

```scala
trait ParserSingletonBridge[+T] {
    final def from(op: Parsley[_]): Parsley[T]
    final def <#(op: Parsley[_]): Parsley[T] = this.from(op)
}
```

The implementation of `from` is not important, it will be handled by the other `ParserBridgeN`s. What these
two combinators give you is the ability to write `Foo.from(parser): Parsley[(Int, Int) => Foo]`, for instance.
This can be useful when you want to use a bridge somewhere where the arguments cannot be directly applied,
like in [chain](expr/chain.md) or [precedence](expr/precedence.md) combinators:

```scala mdoc
import parsley.expr.chain
import parsley.implicits.character.stringLift

val expr = chain.left1(px, Add.from("+")) // or `Add <# "+"`
```

They are analogous to the `as` and `#>` combinators respectively.

### `ParserBridge0[+T]`
This trait is a special case for objects that should return themselves.
As an example, here is an object which forms part of a larger AST, say:

```scala mdoc
import parsley.genericbridges.ParserBridge0
trait Expr
// rest of AST
case object NullLit extends Expr with ParserBridge0[Expr]
```

The `NullLit` object is part of the `Expr` AST, and it has also mixed in `ParserBridge0[Expr]`,
giving it access to `from` and `<#` only (no `apply` for this one!). What this means is that
you can now write the following:

```scala mdoc
val nullLit = NullLit <# "null"
nullLit.parse("null")
```

Without any further configuration, notice that the result of parsing `"null"` is indeed `NullLit`,
and `nullLit: Parsley[Expr]`.

@:callout(error)
Be aware that the type passed to the generic parameter cannot be itself:

```scala mdoc:fail
case object Bad extends ParserBridge0[Bad.type]
```

Resolving this will require introducing an extra type, like `Expr` in the example with
`NullLit`, which breaks the cycle sufficiently.
@:@

## Use Cases

### Normalising or Disambiguating Data

### Enforcing Invariances

@:todo(Talk about the error messages when that's added in 5.0?)

## When *not* to use

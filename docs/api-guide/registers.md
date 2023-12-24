{%
laika.title = "`parsley.registers`"
%}
# Context-Sensitive Parsing (`parsley.registers`)
Normally, context-sensitive parsing can be done with monadic `flatMap`. However,
in `parsley`, `flatMap` is a very expensive operation and is best avoided.
Instead, `parsley` supports a form of arbitrary state threading called *registers*. These can be used to perform context-sensitive parsing in a
more performant way at a cost to expressive power.

@:callout(info)
*The Scaladoc for this page can be found at [`parsley.registers`](@:api(parsley.registers$)).*
@:@

## Registers
A register is a single piece of mutable state threaded through a parser.
They can be made in three different ways:

1.  Importing `parsley.registers.RegisterMaker` allows the use of the `makeReg`
    method on any type `A`:

    ```scala
    def makeReg[B](body: Reg[A] => Parsley[B]): Parsley[B]
    ```

    This will construct a new register filled with the receiver value, and this can be used within the scope of the given continuation. Everytime this is
    executed, it will be uniquely scoped (when the parser is recursive).
2.  Similarly, importing `parsley.registers.RegisterMethods` allows the use of
    the `fillReg` combinator on `Parsley[A]`, which has the same signature as
    `makeReg`.

    It behaves similarly to `makeReg`, but sources its result from a parser.
    Loosely: `p.fillReg(body)` is the same as `p.flatMap(_.makeReg(body))`,
    but is much more efficient.
3.  The `Reg.make[A]: Reg[A]` method allows for the creation of a register
    *outside* of the parsing context. This is not recommended, as it will
    not guarantee uniqueness of scoping. The state will **not** be initialised.

    @:callout(warning)
    Using the same globally constructed register in two places is undefined
    behaviour and should *not* be done.
    @:@

Registers themselves have two core operations, with several more built on top:

```scala
class Reg[A] {
    def get: Parsley[A]
    def put(p: Parsley[A]): Parsley[Unit]
}
```

The `get` method will read the register at parse-time and return the value
contained within. The `put` method takes the result of a parser and stores
that into the register. As examples:

```scala mdoc:to-string
import parsley.character.item
import parsley.registers._

List.empty[Char].makeReg { r1 =>
    item.fillReg { r2 =>
        r1.put(r2.get <::> (r2.get <::> r1.get))
    } *> r1.get
}.parse("a")
```

The above example fills a register `r1`, with the empty list, then fills
a second register `r2` with the result of parsing any character. The value
in `r1` is updated with the list obtained by prepending the parsed character onto
the empty list stored in `r1` twice. After `r2` goes out of scope, the current
value of `r1` is returned.

### Persistence
In the above example, the register `r2` is only used for `get`, but is used
multiple times. Normally, using the value of a parser more than once requires
a `flatMap`; this is not the case in `parsley` when using registers. To make
this application more ergonomic, `parsley.registers.RegisterMethods` also
exposes the `persist` combinator:

```scala mdoc:to-string
List.empty[Char].makeReg { r1 =>
    item.persist { c =>
        r1.put(c <::> (c <::> r1.get))
    } *> r1.get
}.parse("a")
```

Persist can be thought of as a composition of `fillReg` and `get`, or
alternatively as a composition of `flatMap` and `pure`.

### Using State

## Stateful Combinators

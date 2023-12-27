{%
laika.title = "`parsley.registers`"
%}

```scala mdoc:invisible
import parsley.Parsley
```

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

#### Using Persistence
One use of `persist` is to otherwise reduce the scope of an expensive `flatMap`:
the `flatMap` combinator is expensive because it has to process the body of the
function in full everytime it is executed, if the size of the body is reduced,
that will keep the parser faster. Currently, there is no primitive functionality
for parsing with respect to values inside registers, like so:

```scala mdoc:silent
def string(r: Reg[String]): Parsley[String] = r.get.flatMap(parsley.character.string(_))
```

The scope of the `flatMap` in that combinator is small, however, so is likely
much more efficient than one that didn't use persistence. With this, the
context-sensitive parsing of XML tags can be done:

```scala mdoc:to-string
import parsley.Parsley.{atomic, notFollowedBy}
import parsley.character.{stringOfSome, letter}
import parsley.combinator.optional
import parsley.implicits.character.{charLift, stringLift}

val openTag = atomic('<' <~ notFollowedBy('/'))
val tagName = stringOfSome(letter)

lazy val content: Parsley[Unit] = optional(tag)
lazy val tag: Parsley[Unit] = (openTag ~> tagName <~ '>').fillReg { name =>
    content <~ ("</" ~> string(name) <~ ">")
}

tag.parse("<hello></hello>")
tag.parse("<hello></hi>")
tag.parse("<a><b></b></c>")
```

### Long-Term State
Persistence is an example of read-only state used to preserve a value for
later. Writable state can also be used for context-sensitive tasks, by tracking
a value over time. Examples include whitespace-sensitivity or tracking matching
brackets.

#### Matching Brackets
The `local` combinator can be used to reset state after a specific context is
executed. It can be built primitively out of `get`, `put`, and `persist`. The
following parser reports the position of the last unclosed bracket that is
well-interleaved with the other kinds of brackets.

```scala mdoc:to-string
import parsley.character.char
import parsley.combinator.{eof, skipMany}
import parsley.errors.patterns.VerifiedErrors
import parsley.position.pos

case class Brackets(open: Char, position: (Int, Int)) {
    def enter(c: Char, p: (Int, Int)) = {
        val (line, col) = p
        // adjust the column, because it was parsed post-bracket
        this.copy(open = c, position = (line, col-1))
    }
    def missingClose = s"unclosed $open originating at $position"
}
object Brackets {
    def empty = Brackets(0, null) // this will never be matched on
    def toOpen(c: Char) = c match {
        case ')' => '('
        case ']' => '['
        case '}' => '{'
    }
}

def brackets = Brackets.empty.makeReg { bs =>
    def open(c: Char): Parsley[Unit] =
        char(c) ~> bs.modify(pos.map[Brackets => Brackets](p => _.enter(c, p)))
    def close(c: Char): Parsley[Unit] =
        char(c).void | bs.get.verifiedUnexpected(_.missingClose)

    // ensure it is reset
    def scope[A](p: Parsley[A]): Parsley[A] = bs.local(identity[Brackets])(p)

    lazy val matching: Parsley[Unit] = scope {
        skipMany {
              open('(') ~> matching <~ close(')')
            | open('[') ~> matching <~ close(']')
            | open('{') ~> matching <~ close('}')
        }
    }
    matching <~ eof
}
```

The above parser is designed to report where the last unclosed bracket
was. It creates a register `bs` that stores a `Brackets`, which tracks
the last open character and its position. Then, whenever a bracket is
entered, `matching` will save the existing information using the `local`
combinator: giving it the `identity` function will mean it will simply restore
the existing state after it returns. Whenever an open bracket is parsed, it
will write its position into the state (lagging by one character), and then
if the corresponding closing bracket cannot be parsed, it will use an
unconditional [*Verified Error*][*Verified Errors*] to report a message based on the last opened bracket. The results are below:

```scala mdoc:to-string
val p = brackets

p.parse("()()()")
p.parse("[][][]")
p.parse("{}[]()")
p.parse("[[[[[]]]]]")

p.parse("([)]")
p.parse("({(")
p.parse("()[]{[(){}}")
```

Given the relatively simple construction, it works
quite well, and efficiently too: no `flatMap` necessary!

#### Tail-Recursive Combinators
When combinators can be implemented tail recursively instead of
recursively, they can be more efficient. In the context of `parsley`,
tail-recursive combinators are ones which only return the result
of the last recursive call they make:

```scala
lazy val tailRec: Parsley[Unit] = 'a' ~> tailRec | unit
```

The above is tail recursive, for instance. Combinators like `skipMany`
are implemented tail recursively, with additional optimisations to make
them more efficient: implementing new combinators in terms of `skipMany`
with registers to carry state is likely to be efficient. For example:

```scala mdoc:silent
def setOf[A](p: Parsley[A]): Parsley[Set[A]] = {
    Set.empty[A].makeReg { set =>
        skipMany(set.modify(p.map[Set[A] => Set[A]](x => _ + x))) ~> set.get
    }
}
```

In the above code, a set is carried around in a register, and a new element
is added into this set every iteration. When the loop completes (successfully), the set in the register is returned. A more efficient implementation, however,
would use `persist` and a mutable set (along with `impure` and `fresh`): this, of course, still uses a register.

#### Whitespace-Sensitive Languages

## Stateful Combinators

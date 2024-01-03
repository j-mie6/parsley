# Getting Started
Parsley can be added to your project by one of the following configuration commands.

```scala
// SBT
libraryDependencies += "com.github.j-mie6" %% "parsley" % "@VERSION@"

// scala-cli
--dependency com.github.j-mie6::parsley:@VERSION@
// or in file
//> using dep com.github.j-mie6::parsley:@VERSION@

// mill
ivy"com.github.j-mie6::parsley:@VERSION@"
```

## Examples
Parsley can leverage string and character literals in Scala by implicit conversions, keeping the
description of the parser uncluttered:

```scala mdoc:to-string
import parsley.Parsley
import parsley.implicits.character.{charLift, stringLift}

val hello: Parsley[Unit] = ('h' ~> ("ello" | "i") ~> " world!").void
hello.parse("hello world!")
hello.parse("hi world!")
hello.parse("hey world!")
```

Combinators exist to collapse results efficiently for iterative parsers, allowing for concise
definitions of simple numbers:

```scala mdoc:to-string
import parsley.character.digit
val natural: Parsley[Int] = digit.foldLeft1(0)((n, d) => n * 10 + d.asDigit)
natural.parse("0")
natural.parse("123")
```

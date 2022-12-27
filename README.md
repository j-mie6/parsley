# Parsley ![GitHub Workflow Status](https://img.shields.io/github/actions/workflow/status/j-mie6/parsley/ci.yml?branch=master) ![GitHub release](https://img.shields.io/github/v/release/j-mie6/parsley?include_prereleases&sort=semver) [![GitHub license](https://img.shields.io/github/license/j-mie6/parsley.svg)](https://github.com/j-mie6/parsley/blob/master/LICENSE) ![GitHub commits since latest release (by SemVer)](https://img.shields.io/github/commits-since/j-mie6/parsley/latest) [![Badge-Scaladoc]][Link-Scaladoc]

## What is Parsley?
Parsley is a fast and modern parser combinator library for Scala based loosely on a Haskell-style `parsec` API.

## How do I use it? [![parsley Scala version support](https://index.scala-lang.org/j-mie6/parsley/parsley/latest-by-scala-version.svg?platform=jvm)](https://index.scala-lang.org/j-mie6/parsley/parsley) [![parsley Scala version support](https://index.scala-lang.org/j-mie6/parsley/parsley/latest-by-scala-version.svg?platform=sjs1)](https://index.scala-lang.org/j-mie6/parsley/parsley) [![parsley Scala version support](https://index.scala-lang.org/j-mie6/parsley/parsley/latest-by-scala-version.svg?platform=native0.4)](https://index.scala-lang.org/j-mie6/parsley/parsley)

Parsley is distributed on Maven Central, and can be added to your project via:

```scala
libraryDependencies += "com.github.j-mie6" %% "parsley" % "4.0.1"
```

Documentation can be found [**here**](https://javadoc.io/doc/com.github.j-mie6/parsley_2.13/latest/index.html)

If you're a `cats` user, you may also be interested in using [`parsley-cats`](https://github.com/j-mie6/parsley-cats)<a href="https://typelevel.org/cats/"><img src="https://typelevel.org/cats/img/cats-badge.svg" height="40px" align="right" alt="Cats friendly" /></a>
to augment `parsley` with instances for various `cats` typeclasses:

```scala
libraryDependencies += "com.github.j-mie6" %% "parsley-cats" % "0.2.0"
```

### Examples

```scala
scala> import parsley.Parsley
scala> import parsley.implicits.character.{charLift, stringLift}

scala> val hello: Parsley[Unit] = ('h' *> ("ello" <|> "i") *> " world!").void
scala> hello.parse("hello world!")
val res0: parsley.Result[String,Unit] = Success(())
scala> hello.parse("hi world!")
val res1: parsley.Result[String,Unit] = Success(())
scala> hello.parse("hey world!")
val res2: parsley.Result[String,Unit] =
Failure((line 1, column 2):
  unexpected "ey"
  expected "ello"
  >hey world!
    ^^)

scala> import parsley.character.digit
scala> val natural: Parsley[Int] = digit.foldLeft1(0)((n, d) => n * 10 + d.asDigit)
scala> natural.parse("0")
val res3: parsley.Result[String,Int] = Success(0)
scala> natural.parse("123")
val res4: parsley.Result[String,Int] = Success(123)
```

For more see [the Wiki](https://github.com/j-mie6/Parsley/wiki)!

### What are the differences to Haskell's `parsec`?
Mostly, this library is quite similar. However, due to Scala's differences in operator characters a few operators are changed:

* `(<$>)` is known as `map`
* `try` is known as `attempt`
* `(<$)` and `($>)` are `<#` and `#>` respectively.

In addition, `lift2` and `lift3` are uncurried in this library: this is to provide better performance and easier usage with
Scala's traditionally uncurried functions. There are also a few new operators in general to be found here!

## How does it work?
Parsley represents parsers as an abstract-syntax tree AST, which is constructed lazily. As a result, Parsley is able to
perform analysis and optimisations on your parsers, which helps reduce the burden on you, the programmer. This representation
is then compiled into a light-weight stack-based instruction set designed to run fast on the JVM. This is what offers Parsley
its competitive performance, but for best effect a parser should be compiled once and used many times (so-called hot execution).

To make recursive parsers work in this AST format, you must ensure that recursion is done by knot-tying: you should define all
recursive parsers with `val` and introduce `lazy val` where necessary for the compiler to accept the definition.

## Bug Reports [![Percentage of issues still open](https://isitmaintained.com/badge/open/j-mie6/Parsley.svg)](https://isitmaintained.com/project/j-mie6/Parsley "Percentage of issues still open") [![Maintainability](https://img.shields.io/codeclimate/maintainability/j-mie6/Parsley)](https://codeclimate.com/github/j-mie6/Parsley) [![Test Coverage](https://img.shields.io/codeclimate/coverage-letter/j-mie6/Parsley)](https://codeclimate.com/github/j-mie6/Parsley)

If you encounter a bug when using Parsley, try and minimise the example of the parser (and the input) that triggers the bug.
If possible, make a self contained example: this will help me to identify the issue without too much issue.

## References
* This work is based on my Master's Thesis (2018) which can be found [**here**](https://github.com/J-mie6/Parsley/blob/master/parsley.pdf)
* This work spawned a paper at the Scala Symposium at ICFP 2018: [**Garnishing Parsec with Parsley**](https://dl.acm.org/doi/abs/10.1145/3241653.3241656)
* This work supports the patterns introduced at the Scala Symposium in 2022: [**Design Patterns for Parser Combinators in Scala**](https://dl.acm.org/doi/10.1145/3550198.3550427)

<!-- Badges and Links -->


[Link-Scaladoc]: https://javadoc.io/doc/com.github.j-mie6/parsley_2.13/latest/index.html

[Badge-Scaladoc]: https://img.shields.io/badge/documentation-available-green

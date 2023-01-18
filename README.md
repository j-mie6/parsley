# Parsley ![GitHub Workflow Status](https://img.shields.io/github/actions/workflow/status/j-mie6/parsley/ci.yml?branch=master) ![GitHub release](https://img.shields.io/github/v/release/j-mie6/parsley?include_prereleases&sort=semver) [![GitHub license](https://img.shields.io/github/license/j-mie6/parsley.svg)](https://github.com/j-mie6/parsley/blob/master/LICENSE) ![GitHub commits since latest release (by SemVer)](https://img.shields.io/github/commits-since/j-mie6/parsley/latest) [![Badge-Scaladoc]][Link-Scaladoc]

## What is Parsley?
Parsley is a fast and modern parser combinator library for Scala based loosely on a Haskell-style `parsec` API.

## How do I use it? [![parsley Scala version support](https://index.scala-lang.org/j-mie6/parsley/parsley/latest-by-scala-version.svg?platform=jvm)](https://index.scala-lang.org/j-mie6/parsley/parsley) [![parsley Scala version support](https://index.scala-lang.org/j-mie6/parsley/parsley/latest-by-scala-version.svg?platform=sjs1)](https://index.scala-lang.org/j-mie6/parsley/parsley) [![parsley Scala version support](https://index.scala-lang.org/j-mie6/parsley/parsley/latest-by-scala-version.svg?platform=native0.4)](https://index.scala-lang.org/j-mie6/parsley/parsley)

Parsley is distributed on Maven Central, and can be added to your project via:

```scala
libraryDependencies += "com.github.j-mie6" %% "parsley" % "4.1.0-RC1"
```

Documentation can be found [**here**](https://javadoc.io/doc/com.github.j-mie6/parsley_2.13/latest/index.html)

If you're a `cats` user, you may also be interested in using [`parsley-cats`](https://github.com/j-mie6/parsley-cats)<a href="https://typelevel.org/cats/"><img src="https://typelevel.org/cats/img/cats-badge.svg" height="40px" align="right" alt="Cats friendly" /></a>
to augment `parsley` with instances for various `cats` typeclasses:

```scala
libraryDependencies += "com.github.j-mie6" %% "parsley-cats" % "1.1.0"
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

## Library Evolution ![Semantic Versioning: early-semver](https://img.shields.io/badge/version%20policy-early--semver-blue)
Parsley is a _modern_ parser combinator library, which strives to be on the
bleeding-edge of parser combinator library design. This means that improvements
will come naturally over time. Feel free to suggest improvements for
consideration, as well as high-level problems you commonly encounter that we may
be able to find a way to mitigate (see the _Design Patterns for Parser
Combinators_ paper for example!).

### Frequency of Major Changes
Part of innovation is being willing to admit
design mistakes and rectify them: when a binary-breaking release is made, the
opportunity may be taken to polish parts of the libary's API that are clunky, or
could be better organised or improved. For example, see the differences between
`parsley-3.3.10` and `parsley-4.0.0`! However, constant breaking changes are
not a good way to encourage the use of a library as users often want stability:
to that end, annoyances and bugbears with the API are only addressed
approximately _yearly_, and the frequence of these will decrease over time.
For future major releases, care will be taken to, wherever possible, publish
all patch-level changes in a final version to the previous `major.minor`
version, and then all minor-level changes as a final `major.(minor+1).0`
version before releasing the major-level changes as `(major+1).0.0`: this will
allow users stuck on the old version to benefit as much as possible from the
fixes and new functionality.

### Versioning Policy
As of `4.0.0`, `parsley` is _strictly_ commited to `early-semver`, which means
that the version numbers are significant:

* Two versions `x._._` and `y._._` with `x != y` are incompatible with
  each other at a binary level: having `x._._` on the classpath with code
  compiled with the `y._._` will most likely result in a linkage-error at
  runtime.
* Two versions `a.x._` and `a.y._` with `x <= y` are binary compatible, which
  means that code compiled against `a.x._` will still work with `a.y._` on
  the classpath. A "source" component `y > x` indicates that `a.y._` has
  added or deprecated functionality since `a.x._`.
* Two versions `a.b.x` and `a.b.y` are binary and source compatible, which
  means there are no compatiblity concerns between the two versions. Code
  compiled against `a.b.x` will run with `a.b.y` on the classpath and
  vice-versa. A "patch" component `y > x` indicates that `a.b.y` fixes
  issues (bugs or poor performance) with `a.b.x`.

In short, if you are on version `a.x.y`, you can: feel free to upgrade to
version `a.x.z` if `z > y` without worry; and upgrade to `a.z._` if `z > x`,
with a _possible_ (but rare) need to update your code minorly. _Occasionally_,
a "source" component bump may deprecate functionality, but it will provide a
migration to tell you how to avoid the deprecation warning. Altered/deprecated
functionality may be hidden from the public API in a **binary backwards
compatible way** in a "source" bump and therefore may require updating _when
recompiled_; this will be done sparingly and with minimal disruption as to not
discourage updating the libary, and any immediate migration changes to user
code from `a.x._` to _any_ `a.y._` with `y > x` **will** be documented in
`a.y._`'s release.

_Note: all functionality marked as `private [parsley]` or within
the `parsley.internal` package is _not_ adherent to `early-semver` and may be
removed or changed at will with no impact to_ regular/intended _use of the
library._

### Release Candidates and Milestones
Occasionally, a minor (source) release will contain either a significant body of
new work, or a significant rework of some internal machinery. In these cases
additional versioning may be employed:

* Experimental (and volatile) new functionality may be iterated with `a.b.0-Mn`
  versions: these are (hopefully) _working_ pre-release versions of the
  functionality, subject to even binary incompatible changes between `M`
  versions. When the new API and behaviour becomes stable, the release
  graduates to the `a.b.0-RC1` release candidate.
* Release candidates are used to iron-out any lingering issues with a minor
  release and _potentially_ alter the finer-points of the new functionality's
  behaviour. Binary compatiblity will be preserved between `RCx` and `RCy` with
  `y > x` except within truly exceptional circumstances.
* Finally, the release makes it to `a.b.0` and is _hopefully_ truly stable.

### Version EoL (End of Life) Policy
Old versions of the library may still be given important bug-fixes after it
has be obsoleted by a new release. In exceptional circumstances, performance
problems may be addressed for old versions. The lifetime policy is as follows:

* Major (binary) versions reach EoL a minimum of 6 months after its successor
  was released, unless an extension to its life is requested by a issue.
* Minor (source) versions reach EoL immediately on the release of its
  successor, _unless_ deprecations were issued by its successor, in which case
  it will reach EoL after a minimum of 3 months.

Some more minor bugfixes may not be ported to previous versions if they (a) do
not appear in that version or (b) the code has changed too much internally to
make porting feasible.

_An exception to this policy is made for any version `3.x.y`, which reaches EoL effective immediately (December 2022) excluding exceptional circumstances._

| Version | Released On        | EoL Status                  |
|:-------:|:-------------------|:----------------------------|
| `3.3.0` | January 7th 2022   | EoL reached                 |
| `4.0.0` | November 30th 2022 | Enjoying indefinite support |

## Bug Reports [![Percentage of issues still open](https://isitmaintained.com/badge/open/j-mie6/Parsley.svg)](https://isitmaintained.com/project/j-mie6/Parsley "Percentage of issues still open") [![Maintainability](https://img.shields.io/codeclimate/maintainability/j-mie6/parsley)](https://codeclimate.com/github/j-mie6/Parsley) [![Test Coverage](https://img.shields.io/codeclimate/coverage-letter/j-mie6/parsley)](https://codeclimate.com/github/j-mie6/Parsley)

If you encounter a bug when using Parsley, try and minimise the example of the parser (and the input) that triggers the bug.
If possible, make a self contained example: this will help to identify the issue without too much issue.

## How does it work?
Parsley represents parsers as an abstract-syntax tree AST, which is constructed lazily. As a result, Parsley is able to
perform analysis and optimisations on your parsers, which helps reduce the burden on you, the programmer. This representation
is then compiled into a light-weight stack-based instruction set designed to run fast on the JVM. This is what offers Parsley
its competitive performance, but for best effect a parser should be compiled once and used many times (so-called hot execution).

To make recursive parsers work in this AST format, you must ensure that recursion is done by knot-tying: you should define all
recursive parsers with `val` and introduce `lazy val` where necessary for the compiler to accept the definition.

## References
* This work is based on my Master's Thesis (2018) which can be found [**here**](https://github.com/J-mie6/Parsley/blob/master/parsley.pdf)
* This work spawned a paper at the Scala Symposium at ICFP 2018: [**Garnishing Parsec with Parsley**](https://dl.acm.org/doi/abs/10.1145/3241653.3241656)
* This work supports the patterns introduced at the Scala Symposium in 2022: [**Design Patterns for Parser Combinators in Scala**](https://dl.acm.org/doi/10.1145/3550198.3550427)

<!-- Badges and Links -->


[Link-Scaladoc]: https://javadoc.io/doc/com.github.j-mie6/parsley_2.13/latest/index.html

[Badge-Scaladoc]: https://img.shields.io/badge/documentation-available-green

# Parsley ![GitHub Workflow Status](https://img.shields.io/github/workflow/status/j-mie6/parsley/CI) ![GitHub release](https://img.shields.io/github/v/release/j-mie6/parsley?include_prereleases&sort=semver) [![GitHub license](https://img.shields.io/github/license/j-mie6/parsley.svg)](https://github.com/j-mie6/parsley/blob/master/LICENSE)

## What is Parsley?
Parsley is a very fast parser combinator library for Scala based on a Haskell-style Parsec API.

## How do I use it? [![Scaladoc](https://javadoc-badge.appspot.com/com.github.j-mie6/parsley_2.13.svg?label=scaladoc)](https://javadoc-badge.appspot.com/com.github.j-mie6/parsley_2.13) [![Maven Central](https://img.shields.io/maven-central/v/com.github.j-mie6/parsley_2.12?label=maven-central-2.12)](https://mvnrepository.com/artifact/com.github.j-mie6/parsley_2.12) [![Maven Central](https://img.shields.io/maven-central/v/com.github.j-mie6/parsley_2.13?label=maven-central-2.13)](https://mvnrepository.com/artifact/com.github.j-mie6/parsley_2.13) [![Maven Central](https://img.shields.io/maven-central/v/com.github.j-mie6/parsley_3.0.0-M3?label=maven-central-3.0.0-M3)](https://mvnrepository.com/artifact/com.github.j-mie6/parsley_3.0.0-M3) [![Maven Central](https://img.shields.io/maven-central/v/com.github.j-mie6/parsley_0.27?label=maven-central-0.27)](https://mvnrepository.com/artifact/com.github.j-mie6/parsley_0.27)

Parsley is distributed on Maven Central, and can be added to your project via:

```
libraryDependencies += "com.github.j-mie6" %% "parsley" % PARSLEY_VER
```

Documentation can be found [**here**](https://javadoc.io/doc/com.github.j-mie6/parsley_2.13/latest/index.html)

### Examples
TODO

### What are the differences to Haskell's Parsec?
Mostly, this library is quite similar. However, due to Scala's differences in operator characters a few operators are changed:

* `(<$>)` is known as `<#>` or `map`
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

/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley

/** This package contains the definition of 23 basic ''template parser bridge traits'', which
  * are used to implement the ''Parser Bridge'' pattern for types that do not require metadata.
  *
  * The traits within are designed to be extended by the companion object of some case class that
  * is produced as the result of a parser: by using these traits, it enables a new `apply` method
  * that makes it appear like the constructor is applied to the parsers themselves. This can be
  * very useful for performing extra verification on the produced results, or to incorporate metadata
  * into the result. Specifically, these traits are designed to be the bare-minimum functionaity,
  * and do not interact with any metadata.
  *
  * @since 5.0.0
  */
package object templates

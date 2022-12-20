/* SPDX-FileCopyrightText: © 2021 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.implicits

import parsley.Parsley

/** Provides implicit conversions for parsers into unit parsers, and other implicits involving combinators.
  *
  * @since 3.0.0
  */
object combinator {
    // $COVERAGE-OFF$
    /** Drops the result of a parser when required by another combinator.
      *
      * This allows for any value convertible to a parser to have its result set
      * to `Unit` with the `void` combinator.
      *
      * @param p the parser convertible value that should be converted and voided.
      * @param con the witness that the type `P` is converible to a parser.
      * @tparam P the type of the value that is convertible to a parser.
      * @note this doesn't seem to play nicely with Intellij
      * @see [[parsley.Parsley.void `void`]]
      */
    @inline implicit def voidImplicitly[P](p: P)(implicit con: P => Parsley[_]): Parsley[Unit] = con(p).void
    // $COVERAGE-ON$
}

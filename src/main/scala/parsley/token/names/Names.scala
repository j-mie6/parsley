/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.names

import parsley.Parsley, Parsley.pure
import parsley.errors.combinator.{amend, entrench, ErrorMethods, unexpected}

/** TODO:
  *
  * @since 4.0.0
  * @note implementations of this class found within `Lexer` may employ sharing
  *       and refine the `def`s in this class into `val` or `lazy val` when overriding.
  *
  * @define disclaimer
  *   the exact behaviour of this parser is decided by the implementations given in
  *   `Lexer`, which will depend on user-defined configuration. Please see the
  *   relevant documentation of these specific objects.
  */
abstract class Names private[token] {
    /** TODO:
      *
      * @note $disclaimer
      * @since 4.0.0
      */
    def identifier: Parsley[String]
    /** TODO:
      *
      * @note $disclaimer
      * @since 4.0.0
      */
    def userDefinedOperator: Parsley[String] // TODO: rename to operator

    // TODO: definitely want a new combinator for this now
    // TODO: should the explain be here, I could see it making a mess of a precedence table...
    def userDefinedOperator(startChar: Char): Parsley[String] =
        amend {
            entrench(userDefinedOperator).flatMap {
                case x if x.head != startChar => unexpected(s"operator $x")
                case x => pure(x)
            }
        }.explain(s"operator must start with $startChar")
    def userDefinedOperatorEndingIn(endChar: Char): Parsley[String] =
        amend {
            entrench(userDefinedOperator).flatMap {
                case x if x.last != endChar => unexpected(s"operator $x")
                case x => pure(x)
            }
        }.explain(s"operator must end with $endChar")
    def userDefinedOperator(startChar: Char, endChar: Char): Parsley[String] =
        amend {
            entrench(userDefinedOperator).flatMap {
                case x if x.head != startChar && x.last != endChar => unexpected(s"operator $x")
                case x => pure(x)
            }
        }.explain(s"operator must start with $startChar and end with $endChar")
}

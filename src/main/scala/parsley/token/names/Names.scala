/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.names

import parsley.Parsley
import parsley.token.predicate.CharPredicate

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
    // TODO: use other identifier method? disadvantage of this is that we lose out on specialised error message
    def identifier: Parsley[String]
    /** TODO:
      *
      * @note $disclaimer
      * @since 4.0.0
      */
    def identifier(startChar: CharPredicate): Parsley[String]
    /** TODO:
      *
      * @note $disclaimer
      * @since 4.0.0
      */
    def userDefinedOperator: Parsley[String]
    /** TODO:
      *
      * @note $disclaimer
      * @since 4.0.0
      */
    def userDefinedOperator(startChar: CharPredicate, endChar: CharPredicate): Parsley[String]
}

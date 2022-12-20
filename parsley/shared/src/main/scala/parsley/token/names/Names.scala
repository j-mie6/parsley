/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.names

import parsley.Parsley
import parsley.token.predicate.CharPredicate

/** This class defines a uniform interface for defining parsers for user-defined
  * names (identifiers and operators), independent of how whitespace should be
  * handled after the name.
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
    /** This parser will parse an identifier based on the
      * defined identifier start and identifier letter. It
      * is capable of handling unicode characters if the
      * configuration permits. If hard keywords are specified
      * by the configuration, this parser is not permitted
      * to parse them.
      *
      * @example {{{
      * // identifierStart = Basic(_.isLetter)
      * // identifierLetter = Basic(_.isLetterOrDigit)
      * // hardKeywords = Set("if", ...)
      * scala> identifier.parse("x1")
      * val res0 = Success("x1")
      * scala> identifier.parse("1x")
      * val res1 = Failure(...)
      * scala> identifier.parse("")
      * val res2 = Failure(...)
      * scala> identifier.parse("iffy")
      * val res3 = Success("iffy")
      * scala> identifier.parse("if")
      * val res4 = Failure(...)
      * }}}
      *
      * @note $disclaimer
      * @since 4.0.0
      */
    def identifier: Parsley[String]
    /** This combinator will parse an identifier based on the
      * provided identifier start and described identifier letter. It
      * is capable of handling unicode characters if the
      * configuration permits.
      *
      * After parsing a valid identifier as in `identifier`,
      * this combinator will verify that the first character
      * matches the given parameter. If `NotRequired` is passed,
      * this combinator will be equivalent to `identifier`.
      *
      * If hard keywords are specified
      * by the configuration, this parser is not permitted
      * to parse them.
      *
      * @example {{{
      * // identifierStart = Basic(_.isLetter)
      * // identifierLetter = Basic(_.isLetterOrDigit)
      * // hardKeywords = Set("if", ...)
      * scala> identifier(Basic(_.isLower)).parse("x1")
      * val res0 = Success("x1")
      * scala> identifier(Basic(_.isLower)).parse("X1")
      * val res1 = Failure(...)
      * scala> identifier(Basic(_.isLower)).parse("1x")
      * val res2 = Failure(...)
      * scala> identifier(Basic(_.isLower)).parse("")
      * val res3 = Failure(...)
      * scala> identifier(Basic(_.isLower)).parse("iffy")
      * val res4 = Success("iffy")
      * scala> identifier(Basic(_.isLower)).parse("if")
      * val res5 = Failure(...)
      * }}}
      *
      * @param startChar describes what the starting character must be
      * @note $disclaimer
      * @since 4.0.0
      */
    def identifier(startChar: CharPredicate): Parsley[String]
    /** This parser will parse a user-defined operator based on the
      * defined operator start and operator letter. It
      * is capable of handling unicode characters if the
      * configuration permits. If hard operators are specified
      * by the configuration, this parser is not permitted
      * to parse them.
      *
      * @example {{{
      * // operatorStart = Basic(Set('+', '-'))
      * // operatorLetter = Basic(Set('+', '-', ':'))
      * // hardKeywords = Set("+", "+:", ...)
      * scala> userDefinedOperator(NotRequired, Basic(Set(':'))).parse("-:")
      * val res0 = Success("-:")
      * scala> userDefinedOperator(NotRequired, Basic(Set(':'))).parse("*:")
      * val res1 = Failure(...)
      * scala> userDefinedOperator(NotRequired, Basic(Set(':'))).parse("")
      * val res2 = Failure(...)
      * scala> userDefinedOperator(NotRequired, Basic(Set(':'))).parse("++")
      * val res3 = Failure(...)
      * scala> userDefinedOperator(NotRequired, Basic(Set(':'))).parse("+:")
      * val res4 = Failure(...)
      * scala> userDefinedOperator(NotRequired, Basic(Set(':'))).parse("++:")
      * val res5 = Success("++:")
      * }}}
      *
      * @note $disclaimer
      * @since 4.0.0
      */
    def userDefinedOperator: Parsley[String]
    /** This combinator will parse a user-defined operator based on the
      * defined operator start and operator letter, refined by the
      * provided `startChar` and `endChar`. It
      * is capable of handling unicode characters if the
      * configuration permits.
      *
      * After parsing a valid operator as in `userDefinedOperator`,
      * this combinator will verify that the first and last characters
      * match the given parameters. If `NotRequired` is passed to
      * either argument, this will permit any character. Passing
      * it to both arguments will be equivalent to `userDefinedOperator`.
      *
      * If hard operators are specified
      * by the configuration, this parser is not permitted
      * to parse them.
      *
      * @example {{{
      * // operatorStart = Basic(Set('+', '-'))
      * // operatorLetter = Basic(Set('+', '-', ':'))
      * // hardKeywords = Set("+", "+:", ...)
      * scala> identifier.parse("-")
      * val res0 = Success("-")
      * scala> identifier.parse("*")
      * val res1 = Failure(...)
      * scala> identifier.parse("")
      * val res2 = Failure(...)
      * scala> identifier.parse("++")
      * val res3 = Success("++")
      * scala> identifier.parse("+:")
      * val res4 = Failure(...)
      * }}}
      *
      * @param startChar describes what the starting character must be
      * @param endChar  describes what the final character must be
      * @note $disclaimer
      * @since 4.0.0
      */
    def userDefinedOperator(startChar: CharPredicate, endChar: CharPredicate): Parsley[String]
}

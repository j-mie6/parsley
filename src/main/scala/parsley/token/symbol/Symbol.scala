/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.symbol

import scala.language.implicitConversions

import parsley.Parsley
import parsley.errors.combinator.ErrorMethods

/** This class provides implicit functionality to promote string
  * literals into tokens.
  *
  * @since 4.0.0
  */
abstract class ImplicitSymbol private [symbol] {
    /** This method takes the given string and turns it
      * into a parser for that token.
      *
      * This method can be brought into scope in a parser to
      * allow string literals to naturally serve as tokens.
      * In particular, it will correctly deal with known keywords
      * and operators, and otherwise handle other strings at
      * face-value.
      *
      * @note it is assumed that
      * the token's content is irrelevant, since it is
      * already known what it is, so `Unit` is returned.
      *
      * @since 4.0.0
      */
    implicit def implicitSymbol(s: String): Parsley[Unit]
}

abstract class Symbol private[token] {
    /** TODO:
      *
      * @since 4.0.0
      */
    final def apply(name: String, label: String): Parsley[Unit] = apply(name).label(label)
    /** TODO:
      *
      * @todo this does deal with ops/keywords
      * @since 4.0.0
      */
    def apply(name: String): Parsley[Unit]
    /** TODO:
      *
      * @since 4.0.0
      */
    final def apply(name: Char, label: String): Parsley[Unit] = apply(name).label(label)
    /** TODO:
      *
      * @todo talk about how this doesn't check for operators or keywords
      * @since 4.0.0
      */
    def apply(name: Char): Parsley[Unit]

    /** TODO:
      *
      * @since 4.0.0
      */
    def softKeyword(name: String): Parsley[Unit]
    /** TODO:
      *
      * @since 4.0.0
      */
    def operator(name: String): Parsley[Unit]

    /** This object can be imported from to expose a way of converting raw Scala string literals
      * into a parser for that specific token.
      *
      * @since 4.0.0
      */
    final val implicits: ImplicitSymbol = new ImplicitSymbol {
        /** @inheritdoc */
        implicit def implicitSymbol(s: String): Parsley[Unit] = apply(s)
    }

    /** TODO:
      *
      * @since 4.0.0
      */
    final lazy val semi: Parsley[Unit] = apply(';', "semicolon")
    /** TODO:
      *
      * @since 4.0.0
      */
    final lazy val comma: Parsley[Unit] = apply(',', "comma")
    /** TODO:
      *
      * @since 4.0.0
      */
    final lazy val colon: Parsley[Unit] = apply(':', "colon")
    /** TODO:
      *
      * @since 4.0.0
      */
    final lazy val dot: Parsley[Unit] = apply('.', "dot")

    /** TODO:
      *
      * @since 4.0.0
      */
    final lazy val openParen: Parsley[Unit] = apply('(', "open parenthesis")
    /** TODO:
      *
      * @since 4.0.0
      */
    final lazy val openBrace: Parsley[Unit] = apply('{', "open brace")
    /** TODO:
      *
      * @since 4.0.0
      */
    final lazy val openSquare: Parsley[Unit] = apply('[', "open square bracket")
    /** TODO:
      *
      * @since 4.0.0
      */
    final lazy val openAngle: Parsley[Unit] = apply('<', "open angle bracket")
    /** TODO:
      *
      * @since 4.0.0
      */
    final lazy val closingParen: Parsley[Unit] = apply(')', "closing parenthesis")
    /** TODO:
      *
      * @since 4.0.0
      */
    final lazy val closingBrace: Parsley[Unit] = apply('}', "closing brace")
    /** TODO:
      *
      * @since 4.0.0
      */
    final lazy val closingSquare: Parsley[Unit] = apply(']', "closing square bracket")
    /** TODO:
      *
      * @since 4.0.0
      */
    final lazy val closingAngle: Parsley[Unit] = apply('>', "closing angle bracket")
}

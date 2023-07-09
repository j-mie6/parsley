/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley

import parsley.Parsley.{fresh, pure}
import parsley.token.errors.NotConfigured

import parsley.internal.deepembedding.singletons

// TODO: document
object unicode {
    /** This combinator tries to parse a single specific codepoint `c` from the input.
      *
      * Like [[character.char `character.char`]], except it may consume two characters from the input,
      * in the case where the code-point is greater than `0xffff`. This is parsed ''atomically''
      * so that no input is consumed if the first half of the codepoint is parsed and the second
      * is not.
      *
      * @example {{{
      * scala> import parsley.unicode.char
      * scala> char(0x1F643).parse("")
      * val res0 = Failure(..)
      * scala> char(0x1F643).parse("🙂")
      * val res1 = Success(0x1F643)
      * scala> char(0x1F643).parse("b🙂")
      * val res2 = Failure(..)
      * }}}
      *
      * @param c the code-point to parse
      * @return
      * @group core
      */
    def char(c: Int): Parsley[Int] = {
        if (Character.isBmpCodePoint(c)) character.char(c.toChar).as(c)
        else new Parsley(new singletons.SupplementaryCharTok(c, NotConfigured))
    }

    // TODO: document, test
    def satisfy(pred: Int => Boolean): Parsley[Int] = new Parsley(new singletons.UniSatisfy(pred, NotConfigured))

    // TODO: document
    def stringOfMany(pc: Parsley[Int]): Parsley[String] = {
        val pf = pure(addCodepoint(_, _))
        // Can't use the regular foldLeft here, because we need a fresh StringBuilder each time.
        expr.infix.secretLeft1(fresh(new StringBuilder), pc, pf).map(_.toString)
    }

    // TODO: document, test
    def stringOfMany(pred: Int => Boolean): Parsley[String] = stringOfMany(satisfy(pred))

    // TODO: document
    def stringOfSome(pc: Parsley[Int]): Parsley[String] = {
        val pf = pure(addCodepoint(_, _))
        // Can't use the regular foldLeft1 here, because we need a fresh StringBuilder each time.
        expr.infix.secretLeft1(pc.map(addCodepoint(new StringBuilder, _)), pc, pf).map(_.toString)
    }

    // TODO: document, test
    def stringOfSome(pred: Int => Boolean): Parsley[String] = stringOfSome(satisfy(pred))

    private [parsley] def addCodepoint(sb: StringBuilder, codepoint: Int): StringBuilder = {
        if (Character.isSupplementaryCodePoint(codepoint)) {
            sb += Character.highSurrogate(codepoint)
            sb += Character.lowSurrogate(codepoint)
        }
        else sb += codepoint.toChar
    }
}

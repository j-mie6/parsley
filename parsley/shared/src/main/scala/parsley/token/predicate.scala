/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token

import scala.collection.immutable.NumericRange

import parsley.Parsley, Parsley.empty
import parsley.character.{satisfy, satisfyUtf16}
import parsley.exceptions.ParsleyException
import parsley.internal.machine.instructions

// TODO: for parsley 5.0.0, make this a package?
/** This module contains functionality to describe character predicates, which can
  * be used to determine what characters are valid for different tokens.
  *
  * @since 4.0.0
  */
object predicate {
    /** Base class for character predicates.
      * @since 4.0.0
      */
    sealed abstract class CharPredicate {
        private [token] def toBmp: Parsley[Char]
        private [token] def toUnicode: Parsley[Int]
        private [token] def toNative: Parsley[Unit]
        private [token] def startsWith(s: String): Boolean
        private [token] def endsWith(s: String): Boolean
        private [parsley] def asInternalPredicate: instructions.token.CharPredicate
    }

    /** More generic character predicate, which reads any unicode codepoint.
      *
      * Full unicode characters can be up to 24-bits, which is handled by a
      * 32-bit number on the JVM. This predicate can be used, therefore, to
      * handle any single unicode codepoint: this excludes multi-codepoint
      * characters like flags, or modified emojis.
      *
      * In Scala, characters can be upcast to integers, so still can be used
      * in the description of this predicate.
      *
      * @since 4.0.0
      */
    final case class Unicode(predicate: Int => Boolean) extends CharPredicate {
        private [token] override def toBmp = satisfy(c => predicate(c.toInt))
        private [token] override def toUnicode = satisfyUtf16(predicate)
        private [token] override def toNative = toUnicode.void
        private [token] def startsWith(s: String) = s.nonEmpty && predicate(s.codePointAt(0))
        private [token] def endsWith(s: String) = s.nonEmpty && predicate(s.codePointBefore(s.length))
        private [parsley] def asInternalPredicate: instructions.token.CharPredicate = new instructions.token.Unicode(predicate)
    }

    /** Basic character predicate, which reads regular Scala 16-bit characters.
      *
      * This predicate is only capable of recognising characters within the
      * Basic Multilingual Plane.
      *
      * @since 4.0.0
      */
    final case class Basic(predicate: Char => Boolean) extends CharPredicate {
        private [token] override def toBmp = satisfy(predicate)
        // $COVERAGE-OFF$
        private [token] override def toUnicode: Parsley[Int] =
            throw new ParsleyException("Cannot parse unicode with a `Basic` `Char => Boolean` predicate") // scalastyle:ignore throw
        // $COVERAGE-ON$
        private [token] override def toNative = toBmp.void
        private [token] def startsWith(s: String) = s.headOption.exists(predicate)
        private [token] def endsWith(s: String) = s.lastOption.exists(predicate)
        private [parsley] def asInternalPredicate: instructions.token.CharPredicate  = new instructions.token.Basic(predicate)
    }
    // this runs the ability to pass functions in as it creates an overloading ambiguity
    /*object Basic {
        // TODO: expose
        private [parsley] def apply(cs: Char*) = new Basic(Set(cs: _*))
    }*/

    /** Character predicate that never succeeds.
      *
      * @since 4.0.0
      */
    case object NotRequired extends CharPredicate {
        private [token] override def toBmp: Parsley[Char] = empty
        private [token] override def toUnicode: Parsley[Int] = empty
        private [token] override def toNative: Parsley[Unit] = empty
        private [token] def startsWith(s: String) = true
        private [token] def endsWith(s: String) = true
        private [parsley] def asInternalPredicate: instructions.token.CharPredicate  = instructions.token.NotRequired
    }

    /** This object provides implicit functionality for constructing `CharPredicate` values.
      * @since 4.1.0
      */
    object implicits {
        /** Implicit conversions to make `Basic` values.
          * @since 4.1.0
          */
        object Basic {
            // $COVERAGE-OFF$
            /** Lifts a regular character predicate.
              * @since 4.1.0
              */
            implicit def funToBasic(pred: Char => Boolean): CharPredicate = predicate.Basic(pred)
            /** Constructs a predicate for the specific given character.
              * @since 4.1.0
              */
            implicit def charToBasic(c: Char): CharPredicate = predicate.Basic(_ == c)
            /** Constructs a predicate for anything in a range of specific characters.
              * @since 4.1.0
              */
            implicit def rangeToBasic(cs: NumericRange[Char]): CharPredicate = predicate.Basic(cs.contains)
            // $COVERAGE-ON$
        }

        /** Implicit conversions to make `Unicode` values.
          * @since 4.1.0
          */
        object Unicode {
            // $COVERAGE-OFF$
            /** Lifts a regular full-width character predicate.
              * @since 4.1.0
              */
            implicit def funToUnicode(pred: Int => Boolean): CharPredicate = predicate.Unicode(pred)
            /** Lifts a regular character predicate.
              * @since 4.1.0
              */
            implicit def charFunToUnicode(pred: Char => Boolean): CharPredicate = predicate.Unicode(c => c.isValidChar && pred(c.toChar))
            /** Constructs a predicate for the specific given character.
              * @since 4.1.0
              */
            implicit def charToUnicode(c: Char): CharPredicate = predicate.Unicode(_ == c.toInt)
            /** Constructs a predicate for the specific given unicode codepoint.
              * @since 4.1.0
              */
            implicit def intToUnicode(c: Int): CharPredicate = predicate.Unicode(_ == c)
            /** Constructs a predicate for anything in a range of specific characters.
              * @since 4.1.0
              */
            implicit def charRangeToUnicode(cs: NumericRange[Char]): CharPredicate = predicate.Unicode(cs.contains)
            /** Constructs a predicate for anything in a range of specific unicode codepoints.
              * @since 4.1.0
              */
            implicit def intRangeToUnicode(cs: NumericRange[Int]): CharPredicate = predicate.Unicode(cs.contains)
            /** Constructs a predicate for anything in a range of specific unicode codepoints.
              * @since 4.1.0
              */
            implicit def rangeToUnicode(cs: Range): CharPredicate = predicate.Unicode(cs.contains)
            // $COVERAGE-ON$
        }
    }
}

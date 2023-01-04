/* SPDX-FileCopyrightText: © 2021 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token

import scala.collection.immutable.NumericRange

import parsley.Parsley, Parsley.empty
import parsley.character.{satisfy, satisfyUtf16}
import parsley.exceptions.ParsleyException

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
        private [token] override def toUnicode =
            throw new ParsleyException("Cannot parse unicode with a `Basic` `Char => Boolean` predicate") // scalastyle:ignore throw
        // $COVERAGE-ON$
        private [token] override def toNative = toBmp.void
        private [token] def startsWith(s: String) = s.headOption.exists(predicate)
        private [token] def endsWith(s: String) = s.lastOption.exists(predicate)
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
        private [token] override def toBmp = empty
        private [token] override def toUnicode = empty
        private [token] override def toNative = empty
        private [token] def startsWith(s: String) = true
        private [token] def endsWith(s: String) = true
    }

    // TODO: documentation!
    object implicits {
        object Basic {
            // $COVERAGE-OFF$
            implicit def funToBasic(pred: Char => Boolean): CharPredicate = predicate.Basic(pred)
            implicit def charToBasic(c: Char): CharPredicate = predicate.Basic(_ == c)
            implicit def rangeToBasic(cs: NumericRange[Char]): CharPredicate = predicate.Basic(cs.contains)
            // $COVERAGE-ON$
        }

        object Unicode {
            // $COVERAGE-OFF$
            implicit def funToUnicode(pred: Int => Boolean): CharPredicate = predicate.Unicode(pred)
            implicit def charFunToUnicode(pred: Char => Boolean): CharPredicate = predicate.Unicode(c => c.isValidChar && pred(c.toChar))
            implicit def charToUnicode(c: Char): CharPredicate = predicate.Unicode(_ == c.toInt)
            implicit def intToUnicode(c: Int): CharPredicate = predicate.Unicode(_ == c)
            implicit def charRangeToUnicode(cs: NumericRange[Char]): CharPredicate = predicate.Unicode(cs.contains)
            implicit def intRangeToUnicode(cs: NumericRange[Int]): CharPredicate = predicate.Unicode(cs.contains)
            implicit def rangeToUnicode(cs: Range): CharPredicate = predicate.Unicode(cs.contains)
            // $COVERAGE-ON$
        }
    }
}

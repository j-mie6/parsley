/* SPDX-FileCopyrightText: © 2021 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token

import parsley.Parsley, Parsley.empty
import parsley.character.{satisfy, satisfyUtf16}
import parsley.exceptions.ParsleyException

/** TODO:
  * @since 4.0.0
  */
object predicate {
    /** TODO:
      * @since 4.0.0
      */
    sealed abstract class CharPredicate {
        private [token] def toBmp: Parsley[Char]
        private [token] def toUnicode: Parsley[Int]
        private [token] def toNative: Parsley[Unit]
        private [token] def startsWith(s: String): Boolean
        private [token] def endsWith(s: String): Boolean
    }

    /** TODO:
      * @since 4.0.0
      */
    final case class Unicode(predicate: Int => Boolean) extends CharPredicate {
        private [token] override def toBmp = satisfy(c => predicate(c) && !c.isHighSurrogate)
        private [token] override def toUnicode = satisfyUtf16(predicate)
        private [token] override def toNative = toUnicode.void
        private [token] def startsWith(s: String) = s.nonEmpty && predicate(s.codePointAt(0))
        private [token] def endsWith(s: String) = s.nonEmpty && predicate(s.codePointBefore(s.length))
    }

    /** TODO:
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

    /** TODO:
      * @since 4.0.0
      */
    case object NotRequired extends CharPredicate {
        private [token] override def toBmp = empty
        private [token] override def toUnicode = empty
        private [token] override def toNative = empty
        private [token] def startsWith(s: String) = true
        private [token] def endsWith(s: String) = true
    }

    // This has been deprecated, but is still used in the tests, we'll replace it with something else down the line
    // but this is free to remove without affecting bin-compat
    private [parsley] object _CharSet {
        def apply(cs: Set[Char]): CharPredicate = Basic(cs)
        def apply(cs: Char*): CharPredicate = apply(Set(cs: _*))
    }
}

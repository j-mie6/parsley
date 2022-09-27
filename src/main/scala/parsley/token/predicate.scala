/* SPDX-FileCopyrightText: © 2021 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token

import parsley.Parsley, Parsley.empty
import parsley.character.{satisfy, satisfyUtf16}
import parsley.exceptions.ParsleyException

/**
  * @since 4.0.0
  */
object predicate {
    sealed abstract class CharPredicate {
        private [token] def toBmp: Parsley[Char]
        private [token] def toUnicode: Parsley[Int]
        private [token] def toNative: Parsley[Unit]
    }

    final case class Unicode(predicate: Int => Boolean) extends CharPredicate {
        private [token] override def toBmp: Parsley[Char] = satisfy(c => predicate(c.toInt) && !c.isHighSurrogate)
        private [token] override def toUnicode: Parsley[Int] = satisfyUtf16(predicate)
        private [token] override def toNative: Parsley[Unit] = toUnicode.void
    }

    final case class Basic(predicate: Char => Boolean) extends CharPredicate {
        private [token] override def toBmp: Parsley[Char] = satisfy(predicate)
        private [token] override def toUnicode: Parsley[Int] =
            throw new ParsleyException("Cannot parse unicode with a `Basic` `Char => Boolean` predicate") // scalastyle:ignore throw
        private [token] override def toNative: Parsley[Unit] = toBmp.void
    }

    case object NotRequired extends CharPredicate {
        private [token] override def toBmp: Parsley[Char] = empty
        private [token] override def toUnicode: Parsley[Int] = empty
        private [token] override def toNative: Parsley[Unit] = empty
    }

    // This has been deprecated, but is still used in the tests, we'll replace it with something else down the line
    // but this is free to remove without affecting bin-compat
    private [parsley] object _CharSet {
        def apply(cs: Set[Char]): CharPredicate = Basic(cs)
        def apply(cs: Char*): CharPredicate = apply(Set(cs: _*))
    }
}

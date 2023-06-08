/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.errors

import scala.annotation.tailrec
import scala.collection.immutable.WrappedString

private [parsley] object helpers {
    def renderRawString(s: String): String = s match {
        case WhitespaceOrUnprintable(name) => name
        // this will handle utf-16 surrogate pairs properly
        case cs                            => "\"" + cs + "\""
    }

    def combineAsList(elems: List[String]): Option[String] = elems.sorted.reverse match {
        case Nil => None
        case List(alt) => Some(alt)
        case List(alt1, alt2) => Some(s"$alt2 or $alt1")
        // If the result would contains "," then it's probably nicer to preserve any potential grouping using ";"
        case any@(alt::alts) if any.exists(_.contains(",")) => Some(s"${alts.reverse.mkString("; ")}; or $alt")
        case alt::alts => Some(s"${alts.reverse.mkString(", ")}, or $alt")
    }

    object WhitespaceOrUnprintable {
        private final val Newline  = 0x000a
        private final val Carriage = 0x000d
        private final val Tab      = 0x0009
        private final val Space    = 0x0020

        def unapply(cs: Iterable[Char]): Option[String] = unapply(cs.take(2).mkString)
        def unapply(s: String): Option[String] = unapply(s.codePointAt(0))
        def unapply(cp: Int): Option[String] = {
            if (Character.isWhitespace(cp)) cp match {
                case Newline  => Some("newline")
                case Carriage => Some("carriage return")
                case Tab      => Some("tab")
                case Space    => Some("space")
                case _        => Some("whitespace character")
            } else Character.getType(cp) match {
                case Character.FORMAT
                   | Character.SURROGATE
                   | Character.PRIVATE_USE
                   | Character.UNASSIGNED
                   | Character.CONTROL =>
                    Character.toChars(cp) match {
                        case Array(h, l) => Some(f"non-printable codepoint (\\u${h.toInt}%04X\\u${l.toInt}%04X, or 0x$cp%08X)")
                        case Array(c)    => Some(f"non-printable character (\\u${c.toInt}%04X)")
                    }
                case _ => None
            }
        }
    }

    def takeCodePoints(s: WrappedString, n: Int): String = takeCodePoints(s.iterator, n, new StringBuilder)
    def takeCodePoints(s: Iterable[Char], n: Int): String = takeCodePoints(s.iterator, n, new StringBuilder)

    @tailrec private def takeCodePoints(it: Iterator[Char], n: Int, sb: StringBuilder): String = {
        if (n == 0 || !it.hasNext) sb.toString
        else {
            val c = it.next()
            takeCodePoints(it, if (c.isHighSurrogate) n else n - 1, sb += c)
        }
    }
}

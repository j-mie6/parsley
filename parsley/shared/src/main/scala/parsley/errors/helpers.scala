/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.errors

import scala.annotation.tailrec
import scala.collection.immutable.WrappedString
//import scala.util.matching.Regex

// Turn coverage off, because the tests have their own error builder
// We might want to test this on its own though
// $COVERAGE-OFF$
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

    //private val Unprintable: Regex = "(\\p{C})".r

    object WhitespaceOrUnprintable {
        def unapply(cs: Iterable[Char]): Option[String] = unapply(cs.take(2).mkString) /*{
            if (cs.head.isHighSurrogate && cs.size > 1) unapply(Character.toCodePoint(cs.head, cs.tail.head))
            else unapply(cs.head.toInt)
        }*/
        def unapply(s: String): Option[String] = unapply(s.codePointAt(0))
        /*def unapply(c: Char): Option[String] = c match {
            case '\n' => Some("newline")
            case '\t' => Some("tab")
            case c if c.isSpaceChar => Some("space")
            case c if c.isWhitespace => Some("whitespace character")
            case c if c.isHighSurrogate => None
            case Unprintable(up) => Some(f"unprintable character (\\u${up.toInt}%04X)")
            case _ => None
        }*/
        def unapply(cp: Int): Option[String] = {
            if (Character.isWhitespace(cp)) cp match {
                case 0x000a => Some("newline")
                case 0x000d => Some("carriage return")
                case 0x0009 => Some("tab")
                case 0x0020 => Some("space")
                case _      => Some("whitespace character")
            } else Character.getType(cp) match {
                case Character.FORMAT
                   | Character.SURROGATE
                   | Character.PRIVATE_USE
                   | Character.UNASSIGNED
                   | Character.CONTROL =>
                    Character.toChars(cp) match {
                        case Array(h, l) => Some(f"non-printable codepoint (\\u${h.toInt}%04X\\u${l.toInt}%04X, or 0x$cp%08X) ")
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
// $COVERAGE-ON$

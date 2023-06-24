/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.errors

import scala.annotation.tailrec
import scala.collection.immutable.WrappedString

private [parsley] object helpers {
    // These don't really need to be tested right now
    // $COVERAGE-OFF$
    def renderRawString(s: String): String = s match {
        case WhitespaceOrUnprintable(name) => name
        // this will handle utf-16 surrogate pairs properly
        case cs                            => "\"" + cs + "\""
    }

    private def junct(init: List[String], last: String, delim: String, junction: String, oxfordComma: Boolean): String = {
        init.mkString(start = "", sep = delim, end = if (oxfordComma) s"$delim$junction $last" else s" $junction $last")
    }
    private def junct(elems: List[String], junction: String, oxfordComma: Boolean): Option[String] = elems.sorted(Ordering[String].reverse) match {
        case Nil => None
        case List(alt) => Some(alt)
        case List(alt1, alt2) => Some(s"$alt2 $junction $alt1")
        // If the result would contains "," then it's probably nicer to preserve any potential grouping using ";"
        case any@(alt::alts) if any.exists(_.contains(",")) => Some(junct(alts.reverse, alt, delim = "; ", junction = junction, oxfordComma = oxfordComma))
        case alt::alts => Some(junct(alts.reverse, alt, delim = ", ", junction = junction, oxfordComma = true))
    }
    def disjunct(elems: List[String], oxfordComma: Boolean): Option[String] = junct(elems, junction = "or", oxfordComma)
    // $COVERAGE-ON$

    object WhitespaceOrUnprintable {
        // These are all inlined, so aren't "tested"
        // $COVERAGE-OFF$
        private final val Newline  = 0x000a
        private final val Carriage = 0x000d
        private final val Tab      = 0x0009
        private final val Space    = 0x0020
        // $COVERAGE-ON$

        def unapply(cs: Iterable[Char]): Option[String] = cs match {
            case ws: WrappedString => unapply(ws.toString)
            case _                 => unapply(cs.take(2).mkString)
        }
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
                    (Character.toChars(cp): @unchecked) match {
                        case Array(h, l) => Some(f"non-printable codepoint (\\u${h.toInt}%04x\\u${l.toInt}%04x, or 0x$cp%06x)")
                        case Array(c)    => Some(f"non-printable character (\\u${c.toInt}%04x)")
                    }
                case _ => None
            }
        }
    }

    // TODO: optimise this to avoid copy?
    def takeCodePoints(s: WrappedString, n: Int): String = takeCodePoints(s: Iterable[Char], n)
    def takeCodePoints(s: Iterable[Char], n: Int): String = takeCodePoints(s.iterator, n, new StringBuilder)

    @tailrec private def takeCodePoints(it: Iterator[Char], n: Int, sb: StringBuilder): String = {
        if (n == 0 || !it.hasNext) sb.toString
        else {
            val c = it.next()
            takeCodePoints(it, if (c.isHighSurrogate) n else n - 1, sb += c)
        }
    }
}

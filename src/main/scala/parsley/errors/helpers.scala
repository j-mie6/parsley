/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.errors

import scala.util.matching.Regex

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

    private val Unprintable: Regex = "(\\p{C})".r

    object WhitespaceOrUnprintable {
        def unapply(cs: Iterable[Char]): Option[String] = unapply(cs.head)
        def unapply(s: String): Option[String] = unapply(s.charAt(0))
        def unapply(c: Char): Option[String] = c match {
            case '\n' => Some("newline")
            case '\t' => Some("tab")
            case c if c.isSpaceChar => Some("space")
            case c if c.isWhitespace => Some("whitespace character")
            case c if c.isHighSurrogate => None
            case Unprintable(up) => Some(f"unprintable character (\\u${up.toInt}%04X)")
            case _ => None
        }
    }
}
// $COVERAGE-ON$

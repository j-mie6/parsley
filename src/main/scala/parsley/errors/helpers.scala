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
        case cs if cs.head.isWhitespace => cs.head match {
            case c if c.isSpaceChar  => "space"
            case '\n'                => "newline"
            case '\t'                => "tab"
            case _                   => "whitespace character"
        }
        case Unprintable(up) => f"unprintable character (\\u${up.head.toInt}%04X)"
        // Do we want this only in unexpecteds? TODO: This complicates things, honestly...
        case cs              => "\"" + cs.takeWhile(!_.isWhitespace) + "\""
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
}
// $COVERAGE-ON$

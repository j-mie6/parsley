/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.descriptions

import parsley.token.predicate.{CharPredicate, NotRequired}

case class SpaceDesc (commentStart: String,
                      commentEnd: String,
                      commentLine: String,
                      commentLineAllowsEOF: Boolean,
                      nestedComments: Boolean,
                      space: CharPredicate,
                      whitespaceIsContextDependent: Boolean) {
    private [token] lazy val supportsComments = {
        val on = (commentStart.nonEmpty && commentEnd.nonEmpty) || commentLine.nonEmpty
        if (on && commentStart.nonEmpty && commentLine.startsWith(commentStart)) {
            throw new IllegalArgumentException(
                "multi-line comments which are a valid prefix of a single-line comment are not allowed as this causes ambiguity in the parser"
            )
        }
        on
    }
}

object SpaceDesc {
    val plain = SpaceDesc("", "", "", true, false, NotRequired, false)
}

/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.descriptions

import parsley.token.{Impl, NotRequired}

// TODO: These shouldn't just be fixed strings, ideally
// In a /perfect/ world, the commendEnd would depend on the comment start
private [token]
case class SpaceDesc (commentStart: String,
                      commentEnd: String,
                      commentLine: String,
                      commentLineAllowsEOF: Boolean,
                      nestedComments: Boolean,
                      space: Impl) {
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

private [token]
object SpaceDesc {
    val plain = SpaceDesc("", "", "", true, false, NotRequired)
}

/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.descriptions

import parsley.token.predicate.{CharPredicate, NotRequired}

/** This class describes how whitespace should be handled lexically.
  *
  * @param commentStart how do multi-line comments start? (empty for no multi-line comments)
  * @param commentEnd how do multi-line comments end? (empty for no multi-line comments)
  * @param commentLine how do single-line comments start? (empty for no single-line comments)
  * @param commentLineAllowsEOF can a single-line comment be terminated by the end-of-file, or '''must''' it ends with a newline
  * @param nestedComments can multi-line comments be nested within each other?
  * @param space what characters serve as whitespace within the language?
  * @param whitespaceIsContextDependent can the definition of whitespace change depending on context? (in Python, say, newlines are valid whitespace
  *                                     within parentheses, but are significant outside of them)
  * @since 4.0.0
  */
final case class SpaceDesc (commentStart: String,
                            commentEnd: String,
                            commentLine: String,
                            commentLineAllowsEOF: Boolean,
                            nestedComments: Boolean,
                            space: CharPredicate,
                            whitespaceIsContextDependent: Boolean) {
    private [token] lazy val supportsComments = {
        require(commentStart.nonEmpty == commentEnd.nonEmpty, "multi-line comments must describe both start and end")
        val on = commentStart.nonEmpty || commentLine.nonEmpty
        require(commentStart.isEmpty || commentLine.isEmpty || !commentLine.startsWith(commentStart),
                "multi-line comments which are a valid prefix of a single-line comment are not allowed as this causes ambiguity in the parser")
        on
    }
}

/** This object contains any default configurations describing whitespace.
  * @since 4.0.0
  */
object SpaceDesc {
    /** The plain definition of space, with no comments, no nested comments, and no space.
      * @since 4.0.0
      */
    val plain = SpaceDesc("", "", "", true, false, NotRequired, false)
}

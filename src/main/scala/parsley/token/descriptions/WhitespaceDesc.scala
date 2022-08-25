package parsley.token.descriptions

import parsley.token.{Impl, NotRequired}

private [token]
case class WhitespaceDesc (commentStart: String,
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
object WhitespaceDesc {
    val plain = WhitespaceDesc("", "", "", true, false, NotRequired)
}
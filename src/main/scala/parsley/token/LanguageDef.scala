package parsley.token

/**
  * This class is required to construct a TokenParser. It defines the various characteristics of the language to be
  * tokenised. Where a parameter can be either a `Set[Char]` or a `Parsley` object, prefer the `Set` where possible.
  * It will unlock a variety of faster intrinsic versions of the parsers, which will greatly improve tokenisation
  * performance! In addition, the Sets are one time converted to heavily optimised BitSets, though that has up to 8KB
  * memory usage associated but at least doubles the execution speed for that instruction. See `parsley.Impl`.
  *
  * @param commentStart For multi-line comments; how does the comment start? (If this or `commentEnd` is the empty
  *                     string, multi-line comments are disabled)
  * @param commentEnd For multi-line comments; how does the comment end? (If this or `commentEnd` is the empty
  *                   string, multi-line comments are disabled)
  * @param commentLine For single-line comments; how does the comment start? (This this is the empty string, single-line
  *                    comments are disabled)
  * @param nestedComments Are multi-line comments allowed to be nested inside each other? E.g. If `{-` and `-}` are
  *                       opening and closing comments, is the following valid syntax: `{-{-hello -}-}`? Note in C this
  *                       is not the case.
  * @param identStart What characters can an identifier in the language start with?
  * @param identLetter What characters can an identifier in the language consist of after the starting character?
  * @param opStart What characters can an operator in the language start with?
  * @param opLetter What characters can an operator in the language consist of after the starting character?
  * @param keywords What keywords does the language contain?
  * @param operators What operators does the language contain?
  * @param caseSensitive Is the language case-sensitive. I.e. is IF equivalent to if?
  * @param space What characters count as whitespace in the language?
  */
case class LanguageDef (commentStart: String,
                        commentEnd: String,
                        commentLine: String,
                        nestedComments: Boolean,
                        identStart: Impl,
                        identLetter: Impl,
                        opStart: Impl,
                        opLetter: Impl,
                        keywords: Set[String],
                        operators: Set[String],
                        caseSensitive: Boolean,
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
/** This object contains any preconfigured language definitions */
object LanguageDef
{
    val plain = LanguageDef("", "", "", false, NotRequired, NotRequired, NotRequired, NotRequired, Set.empty, Set.empty, true, NotRequired)
}
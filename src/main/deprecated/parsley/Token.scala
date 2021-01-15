package parsley

import parsley.token.TokenSet
import parsley.token.BitSet

// $COVERAGE-OFF$
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
@deprecated("This class will be removed in Parsley 3.0, use `parsley.token.LanguageDef` instead", "v2.2.0")
final case class LanguageDef(commentStart: String,
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
    private [parsley] def adapt: token.LanguageDef = token.LanguageDef(commentStart, commentEnd, commentLine, nestedComments,
                                                                       identStart.adapt, identLetter.adapt,
                                                                       opStart.adapt, opLetter.adapt,
                                                                       keywords, operators, caseSensitive, space.adapt)
}
object LanguageDef
{
    @deprecated("This value will be removed in Parsley 3.0, use `parsley.token.LanguageDef.plan` instead", "v2.2.0")
    val plain = LanguageDef("", "", "", false, NotRequired, NotRequired, NotRequired, NotRequired, Set.empty, Set.empty, true, NotRequired)
}

/**
  * The Impl trait is used to provide implementation of the parser requirements from `LanguageDef`
  */
@deprecated("This trait will be removed in Parsley 3.0, use `parsley.token.Impl` instead", "v2.2.0")
sealed trait Impl {
    private [parsley] def adapt: token.Impl = this match {
        case Parser(p) => token.Parser(p)
        case Predicate(f) => token.Predicate(f)
        case BitSetImpl(cs) => token.BitSetImpl(cs)
        case NotRequired => token.NotRequired
    }
}
/**
  * The implementation provided is a parser which parses the required token.
  * @param p The parser which will parse the token
  */
case class Parser(p: Parsley[_]) extends Impl
/**
  * The implementation provided is a function which matches on the input streams characters
  * @param f The predicate that input tokens are tested against
  */
case class Predicate(f: Char => Boolean) extends Impl
/**
  * This implementation states that the required functionality is not required. If it is used it will raise an error
  * at parse-time
  */
case object NotRequired extends Impl
private [parsley] case class BitSetImpl(cs: TokenSet) extends Impl
/**
  * This implementation uses a set of valid tokens. It is converted to a high-performance BitSet.
  */
object CharSet
{
    /**
      * @param cs The set to convert
      */
    def apply(cs: Set[Char]): Impl = BitSetImpl(new BitSet(Left(cs)))
    def apply(cs: Char*): Impl = apply(Set(cs: _*))
}
/**
  * This implementation uses a predicate to generate a BitSet. This should be preferred over `Predicate` when the
  * function in question is expensive to execute and the parser itself is expected to be used many times. If the
  * predicate is cheap, this is unlikely to provide any performance improvements, but will instead incur heavy space
  * costs
  */
object BitGen
{
    def apply(f: Char => Boolean): Impl = BitSetImpl(new BitSet(Right(f)))
}


/**
  * When provided with a `LanguageDef`, this class will produce a large variety of parsers that can be used for
  * tokenisation of a language. This includes parsing numbers and strings in their various formats and ensuring that
  * all operations consume whitespace after them (so-called lexeme parsers). These are very useful in parsing
  * programming languages. This class also has a large number of hand-optimised intrinsic parsers to improve performance!
  * @param lang The rules that govern the language we are tokenising
  */
@deprecated("This class will be removed in Parsley 3.0, use `parsley.token.Lexer` instead", "v2.2.0")
final class TokenParser(lang: LanguageDef) extends token.Lexer(lang.adapt)
// $COVERAGE-ON$
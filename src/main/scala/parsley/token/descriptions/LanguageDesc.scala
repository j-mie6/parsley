package parsley.token.descriptions

import parsley.token.{Impl, NotRequired}

private [token]
case class LanguageDesc (identStart: Impl,
                         identLetter: Impl,
                         opStart: Impl,
                         opLetter: Impl,
                         keywords: Set[String],
                         operators: Set[String],
                         caseSensitive: Boolean,
                         whitespaceDesc: WhitespaceDesc)

/** This object contains any preconfigured language definitions
  * @since 4.0.0
  */
private [token]
object LanguageDesc {
    val plain = LanguageDesc(NotRequired, NotRequired, NotRequired, NotRequired, Set.empty, Set.empty, true, WhitespaceDesc.plain)
}
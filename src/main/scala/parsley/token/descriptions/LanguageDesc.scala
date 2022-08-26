package parsley.token.descriptions

import parsley.token.{Impl, NotRequired}

private [token]
case class LanguageDesc (identDesc: IdentDesc,
                         opStart: Impl,
                         opLetter: Impl,
                         operators: Set[String],
                         whitespaceDesc: SpaceDesc) {
    private [parsley] def isReservedOp(op: String): Boolean = operators.contains(op)
}

/** This object contains any preconfigured language definitions
  * @since 4.0.0
  */
private [token]
object LanguageDesc {
    val plain = LanguageDesc(IdentDesc.plain, NotRequired, NotRequired, Set.empty, SpaceDesc.plain)
}

package parsley.token.descriptions

import parsley.token.{Impl, NotRequired}

private [token]
case class IdentDesc (identStart: Impl,
                      identLetter: Impl,
                      keywords: Set[String],
                      caseSensitive: Boolean,
                      ) {
    private [parsley] def isReservedName(name: String): Boolean =
        theReservedNames.contains(if (caseSensitive) name else name.toLowerCase)
    private lazy val theReservedNames =  if (caseSensitive) keywords else keywords.map(_.toLowerCase)
}

private [token]
object IdentDesc {
    val plain = IdentDesc(NotRequired, NotRequired, Set.empty, true)
}

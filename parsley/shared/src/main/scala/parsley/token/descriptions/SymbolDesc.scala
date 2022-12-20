/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.descriptions

/** This class describes how symbols (textual literals in a BNF) should be
  * processed lexically.
  *
  * @param hardKeywords what keywords are ''always'' treated as keywords within the language.
  * @param hardOperators what operators are ''always'' treated as reserved operators within the language.
  * @param caseSensitive are the keywords case sensitive: when `false`, `IF == if`.
  * @since 4.0.0
  */
final case class SymbolDesc (hardKeywords: Set[String],
                             hardOperators: Set[String],
                             caseSensitive: Boolean) {
    require((hardKeywords & hardOperators).isEmpty, "there cannot be an intersection between keywords and operators")
    private [parsley] def isReservedName(name: String): Boolean =
        theReservedNames.contains(if (caseSensitive) name else name.toLowerCase)
    private lazy val theReservedNames =  if (caseSensitive) hardKeywords else hardKeywords.map(_.toLowerCase)

    private [parsley] def isReservedOp(op: String): Boolean = hardOperators.contains(op)
}

/** This object contains any preconfigured symbol descriptions.
  * @since 4.0.0
  */
object SymbolDesc {
    /** Plain definition of symbols: case sensitive with no hard keywords or operators.
      * @since 4.0.0
      */
    val plain = SymbolDesc(Set.empty, Set.empty, true)
}

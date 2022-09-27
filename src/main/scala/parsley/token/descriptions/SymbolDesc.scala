/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.descriptions

case class SymbolDesc (hardKeywords: Set[String],
                       hardOperators: Set[String],
                       caseSensitive: Boolean) {
    private [parsley] def isReservedName(name: String): Boolean =
        theReservedNames.contains(if (caseSensitive) name else name.toLowerCase)
    private lazy val theReservedNames =  if (caseSensitive) hardKeywords else hardKeywords.map(_.toLowerCase)

    private [parsley] def isReservedOp(op: String): Boolean = hardOperators.contains(op)
}

object SymbolDesc {
    val plain = SymbolDesc(Set.empty, Set.empty, true)
}

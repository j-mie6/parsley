/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.descriptions

import parsley.token.{Impl, NotRequired}

private [parsley] // TODO: remove
case class LexicalDesc (identDesc: IdentDesc,
                        opStart: Impl,
                        opLetter: Impl,
                        operators: Set[String],
                        numericDesc: numeric.NumericDesc,
                        textDesc: text.TextDesc,
                        spaceDesc: SpaceDesc) {
    private [parsley] def isReservedOp(op: String): Boolean = operators.contains(op)
}

/** This object contains any preconfigured lexical definitions.
  * @since 4.0.0
  */
private [parsley] // TODO: remove
object LexicalDesc {
    val plain = LexicalDesc(IdentDesc.plain, NotRequired, NotRequired, Set.empty, numeric.NumericDesc.plain, text.TextDesc.plain, SpaceDesc.plain)
}

/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.descriptions

import parsley.token.{Impl, NotRequired}

private [parsley] // TODO: remove
case class LanguageDesc (identDesc: IdentDesc,
                         opStart: Impl,
                         opLetter: Impl,
                         operators: Set[String],
                         numericDesc: NumericDesc,
                         textDesc: TextDesc,
                         whitespaceDesc: SpaceDesc) {
    private [parsley] def isReservedOp(op: String): Boolean = operators.contains(op)
}

/** This object contains any preconfigured language definitions
  * @since 4.0.0
  */
private [parsley] // TODO: remove
object LanguageDesc {
    val plain = LanguageDesc(IdentDesc.plain, NotRequired, NotRequired, Set.empty, NumericDesc.plain, TextDesc.plain, SpaceDesc.plain)
}

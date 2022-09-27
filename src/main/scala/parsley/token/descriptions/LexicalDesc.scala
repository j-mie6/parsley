/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.descriptions

case class LexicalDesc (nameDesc: NameDesc,
                        symbolDesc: SymbolDesc,
                        numericDesc: numeric.NumericDesc,
                        textDesc: text.TextDesc,
                        spaceDesc: SpaceDesc)

/** This object contains any preconfigured lexical definitions.
  * @since 4.0.0
  */
object LexicalDesc {
    val plain = LexicalDesc(NameDesc.plain, SymbolDesc.plain, numeric.NumericDesc.plain, text.TextDesc.plain, SpaceDesc.plain)
}

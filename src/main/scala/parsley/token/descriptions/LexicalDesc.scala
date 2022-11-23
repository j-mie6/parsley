/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.descriptions

/** This class describes the aggregation of a bunch of different
  * sub-configurations for lexing a specific language.
  *
  * @param nameDesc the description of name-like lexemes
  * @param symbolDesc the description of specific symbolic lexemes
  * @param numericDesc the description of numeric literals
  * @param textDesc the description of text literals
  * @param spaceDesc the description of whitespace
  * @since 4.0.0
  */
final case class LexicalDesc (nameDesc: NameDesc,
                              symbolDesc: SymbolDesc,
                              numericDesc: numeric.NumericDesc,
                              textDesc: text.TextDesc,
                              spaceDesc: SpaceDesc)

/** This object contains any preconfigured lexical definitions.
  * @since 4.0.0
  */
object LexicalDesc {
    /** Defaults to the plain definitions of each sub-description.
      * @since 4.0.0
      */
    val plain = LexicalDesc(NameDesc.plain, SymbolDesc.plain, numeric.NumericDesc.plain, text.TextDesc.plain, SpaceDesc.plain)
}

/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.text

import parsley.Parsley
import parsley.character.satisfy

/** TODO:
  *
  * @since 4.0.0
  */
abstract class Character private[token] {
    /** TODO:
      *
      * @since 4.0.0
      */
    def unicode: Parsley[Int]
    /** TODO:
      *
      * @since 4.0.0
      */
    def basicMultilingualPlane: Parsley[Char]
    /** TODO:
      *
      * @since 4.0.0
      */
    def ascii: Parsley[Char]
    /** TODO:
      *
      * @since 4.0.0
      */
    def extendedAscii: Parsley[Char]
}

private [text] object Character {
    final val MaxAscii: Int = 0x7f
    final val MaxExtendedAscii: Int = 0xff

    def letter(terminalLead: Char, allowsAllSpace: Boolean, isGraphic: Char => Boolean): Char => Boolean = {
        if (allowsAllSpace) c => c != terminalLead && (isGraphic(c) || parsley.character.isWhitespace(c))
        else                c => c != terminalLead && isGraphic(c)
    }

    def letter(terminalLead: Char, escapeLead: Char, allowsAllSpace: Boolean, isGraphic: Char => Boolean): Char => Boolean = {
        if (allowsAllSpace) c => c != terminalLead && c != escapeLead && (isGraphic(c) || parsley.character.isWhitespace(c))
        else                c => c != terminalLead && c != escapeLead && isGraphic(c)
    }

    @inline def isSurrogatePair(high: Char, low: Char): Boolean = java.lang.Character.isSurrogatePair(high, low)
    @inline def isBmpCodePoint(codepoint: Int): Boolean = java.lang.Character.isBmpCodePoint(codepoint)
    @inline def toCodePoint(high: Char, low: Char): Int = java.lang.Character.toCodePoint(high, low)
    @inline def toChars(codepoint: Int): Array[Char] = java.lang.Character.toChars(codepoint)
    @inline def isValidCodePoint(codepoint: Int): Boolean = java.lang.Character.isValidCodePoint(codepoint)
}

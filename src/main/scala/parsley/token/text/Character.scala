/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.text

import parsley.Parsley

abstract class Character private[token] {
    def unicode: Parsley[Int]
    def basicMultilingualPlane: Parsley[Char]
    def ascii: Parsley[Char]
    def extendedAscii: Parsley[Char]
}

private [text] object Character {
    final val MaxAscii: Int = 0x7f
    final val MaxExtendedAscii: Int = 0xff

    @inline def isSurrogatePair(high: Char, low: Char): Boolean = java.lang.Character.isSurrogatePair(high, low)
    @inline def isBmpCodePoint(codepoint: Int): Boolean = java.lang.Character.isBmpCodePoint(codepoint)
    @inline def toCodePoint(high: Char, low: Char): Int = java.lang.Character.toCodePoint(high, low)
    @inline def toChars(codepoint: Int): Array[Char] = java.lang.Character.toChars(codepoint)
}

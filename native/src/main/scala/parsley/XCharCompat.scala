/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley

private [parsley] object XCharCompat {
    private def highSurrogateFromNormalised(cp: Int): Char = (0xd800 | ((cp >> 10) & 0x3ff)).toChar
    private def lowSurrogateFromNormalised(cp: Int): Char = (0xdc00 | (cp & 0x3ff)).toChar

    def highSurrogate(codepoint: Int): Char = highSurrogateFromNormalised(codepoint - 0x10000)
    def lowSurrogate(codepoint: Int): Char = lowSurrogateFromNormalised(codepoint - 0x10000)
}

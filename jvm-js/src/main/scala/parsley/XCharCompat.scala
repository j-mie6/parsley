/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley

private [parsley] object XCharCompat {
    def highSurrogate(codepoint: Int): Char = Character.highSurrogate(codepoint)
    def lowSurrogate(codepoint: Int): Char = Character.lowSurrogate(codepoint)
}

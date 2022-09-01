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

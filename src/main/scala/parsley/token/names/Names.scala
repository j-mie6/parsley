/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.names

import parsley.Parsley

abstract class Names private[token] {
    def identifier: Parsley[String]
    def userOp: Parsley[String] // TODO: rename to operator
    def reservedOp: Parsley[String] // TODO: remove?

    // TODO: we want to add functionality for operator starting with a given character
    //       ending with given character, and starting and ending with given character
}

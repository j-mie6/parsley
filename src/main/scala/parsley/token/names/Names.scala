/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.names

import parsley.Parsley

abstract class Names private[token] {
    def identifier: Parsley[String]
    def userOp: Parsley[String]
    def reservedOp: Parsley[String]
}

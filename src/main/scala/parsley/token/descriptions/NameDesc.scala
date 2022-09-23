/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.descriptions

import parsley.token.{Impl, NotRequired}

case class NameDesc (identStart: Impl,
                     identLetter: Impl,
                     opStart: Impl,
                     opLetter: Impl)

object NameDesc {
    val plain = NameDesc(NotRequired, NotRequired, NotRequired, NotRequired)
}

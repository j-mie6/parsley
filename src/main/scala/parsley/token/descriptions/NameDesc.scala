/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.descriptions

import parsley.token.{Impl, NotRequired}

// TODO: I think it's important to move to proper unicode handling of these
//       We'll want `Int => Boolean` functions instead of `Impl`, which should
//       go.
case class NameDesc (identifierStart: Impl,
                     identifierLetter: Impl,
                     operatorStart: Impl,
                     operatorLetter: Impl)

object NameDesc {
    val plain = NameDesc(NotRequired, NotRequired, NotRequired, NotRequired)
}

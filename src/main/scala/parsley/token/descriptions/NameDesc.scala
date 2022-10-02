/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.descriptions

import parsley.token.predicate.{CharPredicate, NotRequired}

final case class NameDesc (identifierStart: CharPredicate,
                           identifierLetter: CharPredicate,
                           operatorStart: CharPredicate,
                           operatorLetter: CharPredicate)

object NameDesc {
    val plain = NameDesc(NotRequired, NotRequired, NotRequired, NotRequired)
}

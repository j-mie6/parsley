/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.descriptions

import parsley.token.predicate.{CharPredicate, NotRequired}

/** The class describes how name-like things are described lexically.
  *
  * @param identifierStart what characters may start an identifier?
  * @param identifierLetter what characters may continue an identifier?
  * @param operatorStart what characters may start a user-defined operator?
  * @param operatorLetter what characters may continue a user-defined operator?
  * @since 4.0.0
  */
final case class NameDesc (identifierStart: CharPredicate,
                           identifierLetter: CharPredicate,
                           operatorStart: CharPredicate,
                           operatorLetter: CharPredicate)

/** This object contains any preconfigured name definitions.
  * @since 4.0.0
  */
object NameDesc {
    /** Plain description of names, where neither identifiers nor operators are required.
      *
      * {{{
      * identifierStart = NotRequired
      * identifierLetter = NotRequired
      * operatorStart = NotRequired
      * operatorLetter = NotRequired
      * }}}
      *
      * @since 4.0.0
      */
    val plain = NameDesc(identifierStart = NotRequired, identifierLetter = NotRequired, operatorStart = NotRequired, operatorLetter = NotRequired)
}

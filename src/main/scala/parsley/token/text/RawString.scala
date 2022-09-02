/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.text

import scala.Predef.{String => ScalaString, _}

import parsley.Parsley, Parsley.{empty, fresh, pure}
import parsley.character.{satisfy, char}
import parsley.combinator.{between, skipSome}
import parsley.errors.combinator.{amend, entrench, ErrorMethods}
import parsley.implicits.character.charLift
import parsley.token.descriptions.TextDesc

import parsley.internal.deepembedding.singletons

private [token] final class RawString(desc: TextDesc) extends String {
    override lazy val unicode: Parsley[ScalaString] = new Parsley(singletons.RawStringLiteral)
    override lazy val ascii: Parsley[ScalaString] = String.ensureAscii(unicode)
    override lazy val extendedAscii: Parsley[ScalaString] = String.ensureExtendedAscii(unicode)
}

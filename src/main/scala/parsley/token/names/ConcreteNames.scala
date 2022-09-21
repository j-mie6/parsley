/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.names

import parsley.Parsley, Parsley.{attempt, pure}
import parsley.character.stringOfMany
import parsley.errors.combinator.{amend, entrench, ErrorMethods, unexpected}
import parsley.lift.lift2
import parsley.token.{Impl, Static}
import parsley.token.descriptions.LexicalDesc
import parsley.internal.deepembedding.singletons

private [token] class ConcreteNames(desc: LexicalDesc, identStart: Parsley[Char], identLetter: Parsley[Char], opStart: Parsley[Char], opLetter: Parsley[Char])
    extends Names {
    private def keyOrOp(startImpl: Impl, letterImpl: Impl, parser: =>Parsley[String], illegal: String => Boolean,
                        combinatorName: String, name: String, illegalName: String) = {
        val builder = (start: Char => Boolean, letter: Char => Boolean) =>
            new Parsley(new singletons.NonSpecific(combinatorName, name, illegalName, start, letter, illegal))
        (startImpl, letterImpl) match {
            case (Static(start), Static(letter)) => builder(start, letter)
            case _ =>
                attempt {
                    amend {
                        // TODO: Ideally, we'd make a combinator that eliminates this flatMap
                        entrench(parser).flatMap {
                            case x if illegal(x) => unexpected(s"$illegalName $x")
                            case x => pure(x)
                        }
                    }
                }.label(name)
        }
    }
    private lazy val ident = lift2((c: Char, cs: String) => s"$c$cs", identStart, stringOfMany(identLetter))
    private lazy val oper = lift2((c: Char, cs: String) => s"$c$cs", opStart, stringOfMany(opLetter))
    override lazy val identifier: Parsley[String] =
        keyOrOp(desc.identDesc.identStart, desc.identDesc.identLetter, ident, desc.identDesc.isReservedName(_),  "identifier", "identifier", "keyword")
    override lazy val userOp: Parsley[String] =
        keyOrOp(desc.opStart, desc.opLetter, oper, desc.isReservedOp(_), "userOp", "operator", "reserved operator")
    override lazy val reservedOp: Parsley[String] =
        keyOrOp(desc.opStart, desc.opLetter, oper, !desc.isReservedOp(_), "reservedOp", "operator", "non-reserved operator")
}

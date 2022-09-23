/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.names

import parsley.Parsley, Parsley.{attempt, pure}
import parsley.character.stringOfMany
import parsley.errors.combinator.{amend, entrench, ErrorMethods, unexpected}
import parsley.lift.lift2
import parsley.token.{Impl, Static}
import parsley.token.descriptions.{NameDesc, SymbolDesc}
import parsley.internal.deepembedding.singletons

private [token] class ConcreteNames(nameDesc: NameDesc, symbolDesc: SymbolDesc,
                                    identStart: =>Parsley[Char], identLetter: =>Parsley[Char],
                                    opStart: =>Parsley[Char], opLetter: =>Parsley[Char]) extends Names {
    private def keyOrOp(startImpl: Impl, letterImpl: Impl, parser: =>Parsley[String], illegal: String => Boolean,
                        combinatorName: String, name: String, illegalName: String) = {
        val builder = (start: Char => Boolean, letter: Char => Boolean) =>
            new Parsley(new singletons.NonSpecific(combinatorName, name, illegalName, start, letter, illegal))
        (startImpl, letterImpl) match {
            case (Static(start), Static(letter)) => builder(start, letter)
            case _ =>
                attempt {
                    amend {
                        entrench(parser).unexpectedWhen {
                            case x if illegal(x) => s"$illegalName $x"
                        }
                    }
                }.label(name)
        }
    }
    private lazy val ident = lift2((c: Char, cs: String) => s"$c$cs", identStart, stringOfMany(identLetter))
    private lazy val oper = lift2((c: Char, cs: String) => s"$c$cs", opStart, stringOfMany(opLetter))
    override lazy val identifier: Parsley[String] =
        keyOrOp(nameDesc.identStart, nameDesc.identLetter, ident, symbolDesc.isReservedName(_),  "identifier", "identifier", "keyword")
    override lazy val userDefinedOperator: Parsley[String] =
        keyOrOp(nameDesc.opStart, nameDesc.opLetter, oper, symbolDesc.isReservedOp(_), "userOp", "operator", "reserved operator")

    def userDefinedOperator(startChar: Option[Char], endChar: Option[Char]): Parsley[String] = attempt {
        amend {
            entrench(userDefinedOperator).unexpectedWhen {
                case x if startChar.forall(x.head != _) && endChar.forall(x.last != _) => s"operator $x"
            }
        }
    }
}

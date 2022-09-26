/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.names

import parsley.Parsley, Parsley.{attempt, pure}
import parsley.character.stringOfMany
import parsley.errors.combinator.{amend, entrench, ErrorMethods, unexpected}
import parsley.lift.lift2
import parsley.token.{Impl, Basic}
import parsley.token.descriptions.{NameDesc, SymbolDesc}
import parsley.internal.deepembedding.singletons

private [token] class ConcreteNames(nameDesc: NameDesc, symbolDesc: SymbolDesc) extends Names {
    private def keyOrOp(startImpl: Impl, letterImpl: Impl, parser: =>Parsley[String], illegal: String => Boolean,
                        combinatorName: String, name: String, illegalName: String) = {
        (startImpl, letterImpl) match {
            case (Basic(start), Basic(letter)) => new Parsley(new singletons.NonSpecific(combinatorName, name, illegalName, start, letter, illegal))
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
    // FIXME: These need to account for unicodeness, not just BMP
    private lazy val identStart = nameDesc.identifierStart.toBmp
    private lazy val identLetter = nameDesc.identifierLetter.toBmp
    private lazy val ident = lift2((c: Char, cs: String) => s"$c$cs", identStart, stringOfMany(identLetter))
    private lazy val opStart = nameDesc.operatorStart.toBmp
    private lazy val opLetter = nameDesc.operatorLetter.toBmp
    private lazy val oper = lift2((c: Char, cs: String) => s"$c$cs", opStart, stringOfMany(opLetter))
    override lazy val identifier: Parsley[String] =
        keyOrOp(nameDesc.identifierStart, nameDesc.identifierLetter, ident, symbolDesc.isReservedName(_),  "identifier", "identifier", "keyword")
    override lazy val userDefinedOperator: Parsley[String] =
        keyOrOp(nameDesc.operatorStart, nameDesc.operatorLetter, oper, symbolDesc.isReservedOp(_), "userOp", "operator", "reserved operator")

    def userDefinedOperator(startChar: Option[Char], endChar: Option[Char]): Parsley[String] = attempt {
        amend {
            entrench(userDefinedOperator).unexpectedWhen {
                case x if startChar.forall(x.head != _) && endChar.forall(x.last != _) => s"operator $x"
            }
        }
    }
}

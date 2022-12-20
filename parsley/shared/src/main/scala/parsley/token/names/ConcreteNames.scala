/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.names

import parsley.Parsley, Parsley.{attempt, empty, pure}
import parsley.character.{satisfy, satisfyUtf16, stringOfMany, stringOfManyUtf16}
import parsley.errors.combinator.ErrorMethods
import parsley.implicits.zipped.Zipped2
import parsley.token.descriptions.{NameDesc, SymbolDesc}
import parsley.token.predicate.{Basic, CharPredicate, NotRequired, Unicode}

import parsley.internal.deepembedding.singletons

private [token] class ConcreteNames(nameDesc: NameDesc, symbolDesc: SymbolDesc) extends Names {
    private def keyOrOp(startImpl: CharPredicate, letterImpl: CharPredicate, parser: =>Parsley[String], illegal: String => Boolean,
                        combinatorName: String, name: String, illegalName: String) = {
        (startImpl, letterImpl) match {
            case (Basic(start), Basic(letter)) => new Parsley(new singletons.NonSpecific(combinatorName, name, illegalName, start, letter, illegal))
            case _ =>
                attempt {
                    parser.unexpectedWhen {
                        case x if illegal(x) => s"$illegalName $x"
                    }
                }.label(name)
        }
    }
    private def trailer(impl: CharPredicate) = impl match {
        case Basic(letter) => stringOfMany(satisfy(letter))
        case Unicode(letter) => stringOfManyUtf16(satisfyUtf16(letter))
        case NotRequired => pure("")
    }
    private def complete(start: CharPredicate, letter: CharPredicate) = start match {
        case Basic(start) => (satisfy(start), trailer(letter)).zipped((c, cs) => s"$c$cs")
        case Unicode(start) => (satisfyUtf16(start), trailer(letter)).zipped { (c, cs) =>
            if (Character.isSupplementaryCodePoint(c)) s"${Character.highSurrogate(c)}${Character.lowSurrogate(c)}$cs"
            else s"${c.toChar}$cs"
        }
        case NotRequired => empty
    }
    private lazy val ident = complete(nameDesc.identifierStart, nameDesc.identifierLetter)
    private lazy val oper = complete(nameDesc.operatorStart, nameDesc.operatorLetter)
    override lazy val identifier: Parsley[String] =
        keyOrOp(nameDesc.identifierStart, nameDesc.identifierLetter, ident, symbolDesc.isReservedName(_),  "identifier", "identifier", "keyword")
    override def identifier(startChar: CharPredicate): Parsley[String] = attempt {
        identifier.unexpectedWhen {
            case x if !startChar.startsWith(x) => s"identifier $x"
        }
    }

    override lazy val userDefinedOperator: Parsley[String] =
        keyOrOp(nameDesc.operatorStart, nameDesc.operatorLetter, oper, symbolDesc.isReservedOp(_), "userOp", "operator", "reserved operator")

    def userDefinedOperator(startChar: CharPredicate, endChar: CharPredicate): Parsley[String] = attempt {
        userDefinedOperator.unexpectedWhen {
            case x if !startChar.startsWith(x) || !endChar.endsWith(x) => s"operator $x"
        }
    }
}

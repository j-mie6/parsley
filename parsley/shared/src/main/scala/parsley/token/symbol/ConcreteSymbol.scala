/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.symbol

import parsley.Parsley, Parsley.attempt
import parsley.character.{char, string}
import parsley.token.descriptions.{NameDesc, SymbolDesc}
import parsley.token.errors.ErrorConfig

import parsley.internal.deepembedding.singletons.token

private [token] class ConcreteSymbol(nameDesc: NameDesc, symbolDesc: SymbolDesc, err: ErrorConfig) extends Symbol(err) {

    override def apply(name: String): Parsley[Unit] = {
        require(name.nonEmpty, "Symbols may not be empty strings")
        if (symbolDesc.hardKeywords(name))       softKeyword(name)
        else if (symbolDesc.hardOperators(name)) softOperator(name)
        else                                     attempt(string(name)).void
    }

    override def apply(name: Char): Parsley[Unit] = char(name).void

    /*
    private lazy val identLetter = nameDesc.identifierLetter.toNative
    private def caseChar(c: Int) = if (Character.isLetter(c)) charUtf16(Character.toLowerCase(c)) <|> charUtf16(Character.toUpperCase(c)) else charUtf16(c)
    private def caseString(name: String): Parsley[Unit] = {
        if (symbolDesc.caseSensitive) string(name).void
        else {
            val len = name.length
            var offset = 0
            var p = unit
            while (offset < len) {
                val codepoint = name.codePointAt(offset)
                p <~= caseChar(codepoint)
                offset += Character.charCount(codepoint)
            }
            p
        }.label(name)
    }
    override def softKeyword(name: String): Parsley[Unit] = {
        require(name.nonEmpty, "Keywords may not be empty strings")
        attempt {
            err.labelSymbolKeyword(name)(caseString(name)) *>
            notFollowedBy(identLetter).label(err.labelSymbolEndOfKeyword(name))
        }
    }
    */

    override def softKeyword(name: String): Parsley[Unit] = {
        require(name.nonEmpty, "Keywords may not be empty strings")
        new Parsley(new token.SoftKeyword(name, nameDesc.identifierLetter, symbolDesc.caseSensitive,
                                          err.labelSymbolKeyword(name), err.labelSymbolEndOfKeyword(name)))
    }

    //private lazy val opLetter = nameDesc.operatorLetter.toNative
    override def softOperator(name: String): Parsley[Unit] = {
        require(name.nonEmpty, "Operators may not be empty strings")
        new Parsley(new token.SoftOperator(name, nameDesc.operatorLetter, symbolDesc.hardOperatorsTrie,
                                           err.labelSymbolOperator(name), err.labelSymbolEndOfOperator(name)))
        /*val ends = symbolDesc.hardOperators.collect {
            case op if op.startsWith(name) && op != name => op.substring(name.length)
        }.toList
        ends match {
            case Nil => attempt {
                err.labelSymbolOperator(name)(string(name)) *>
                notFollowedBy(opLetter).label(err.labelSymbolEndOfOperator(name))
            }
            case end::ends => attempt {
                err.labelSymbolOperator(name)(string(name)) *>
                notFollowedBy(opLetter <|> strings(end, ends: _*)).label(err.labelSymbolEndOfOperator(name))
            }
        }*/
    }
}

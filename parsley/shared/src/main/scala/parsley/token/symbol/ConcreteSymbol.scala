/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.symbol

import parsley.Parsley, Parsley.{attempt, notFollowedBy, unit}
import parsley.character.{char, string, strings}
import parsley.errors.combinator.ErrorMethods
import parsley.token.descriptions.{NameDesc, SymbolDesc}
import parsley.token.predicate.Basic
import parsley.token.errors.ErrorConfig

import parsley.internal.deepembedding.singletons

private [token] class ConcreteSymbol(nameDesc: NameDesc, symbolDesc: SymbolDesc, err: ErrorConfig) extends Symbol(err) {
    private lazy val identLetter = nameDesc.identifierLetter.toNative
    private lazy val opLetter = nameDesc.operatorLetter.toNative

    override def apply(name: String): Parsley[Unit] = {
        if (symbolDesc.hardKeywords(name))       softKeyword(name)
        else if (symbolDesc.hardOperators(name)) softOperator(name)
        else                                     attempt(string(name)).void
    }

    override def apply(name: Char): Parsley[Unit] = char(name).void

    private def caseString(name: String): Parsley[Unit] = {
        def caseChar(c: Char): Parsley[Char] = if (c.isLetter) char(c.toLower) <|> char(c.toUpper) else char(c)
        if (symbolDesc.caseSensitive) string(name).void
        else name.foldLeft(unit)((p, c) => p <* caseChar(c)).label(name)
    }

    // TODO: We might want to memoise this, but it must be done thread-safely: synchronising on the maps should be enough
    override def softKeyword(name: String): Parsley[Unit] = nameDesc.identifierLetter match {
        // TODO: this needs optimising for Unicode
        case Basic(letter) =>
            new Parsley(new singletons.Specific(name, err.labelSymbolKeyword(name), err.labelSymbolEndOfKeyword(name), letter, symbolDesc.caseSensitive))
        case _ => attempt {
            err.labelSymbolKeyword(name)(caseString(name)) *>
            notFollowedBy(identLetter).label(err.labelSymbolEndOfKeyword(name))
        }
    }

    override def softOperator(name: String): Parsley[Unit] = /*nameDesc.operatorLetter match*/ {
        //case _ =>
            val ends = symbolDesc.hardOperators.collect {
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
            }

    }
}

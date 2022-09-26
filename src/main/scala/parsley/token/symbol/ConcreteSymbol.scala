/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.symbol

// TODO: This can be enabled later, when finalised: js-native will need to not use this
//import scala.collection.concurrent

import parsley.Parsley, Parsley.{attempt, notFollowedBy, pure, unit}
import parsley.character.{char, string, strings}
import parsley.errors.combinator.ErrorMethods
import parsley.token.Basic
import parsley.token.descriptions.{NameDesc, SymbolDesc}

import parsley.internal.deepembedding.singletons

private [token] class ConcreteSymbol(nameDesc: NameDesc, symbolDesc: SymbolDesc) extends Symbol {
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

    //private val keywordMemo = concurrent.TrieMap.empty[String, Parsley[Unit]]
    override def softKeyword(name: String): Parsley[Unit] = /*keywordMemo.getOrElseUpdate(name, */nameDesc.identifierLetter match {
        case Basic(letter) => new Parsley(new singletons.Specific("keyword", name, letter, symbolDesc.caseSensitive))
        case letter => attempt(caseString(name) *> notFollowedBy(identLetter).label(s"end of $name"))
    }//)

    //private val operatorMemo = concurrent.TrieMap.empty[String, Parsley[Unit]]
    override def softOperator(name: String): Parsley[Unit] = /*operatorMemo.getOrElseUpdate(name, */nameDesc.operatorLetter match {
        // TODO: this needs optimising
        //case Static(letter) => new Parsley(new singletons.Specific("operator", name, letter, true))
        case _ =>
            val ends = symbolDesc.hardOperators.collect {
                case op if op.startsWith(name) && op != name => op.substring(name.length)
            }.toList
            ends match {
                case Nil => attempt(string(name) *> notFollowedBy(opLetter).label(s"end of $name"))
                case end::ends => attempt(string(name) *> notFollowedBy(opLetter <|> strings(end, ends: _*)).label(s"end of $name"))
            }

    }//)
}

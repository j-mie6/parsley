/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.symbol

//import java.util.concurrent.ConcurrentHashMap

import scala.collection.concurrent

import parsley.Parsley, Parsley.{attempt, notFollowedBy, pure}
import parsley.character.{char, string}
import parsley.errors.combinator.ErrorMethods
import parsley.token.Static
import parsley.token.descriptions.LexicalDesc

import parsley.internal.deepembedding.singletons

private [token] class ConcreteSymbol(desc: LexicalDesc, identLetter: Parsley[Char], opLetter: Parsley[Char]) extends Symbol {

    override def apply(name: String): Parsley[Unit] = {
        if (desc.identDesc.hardKeywords(name)) softKeyword(name)
        else if (desc.operators(name))         maxOp(name)
        else                                   attempt(string(name)).void
    }

    override def apply(name: Char): Parsley[Unit] = char(name).void


    private def caseString(name: String): Parsley[String] = {
        def caseChar(c: Char): Parsley[Char] = if (c.isLetter) char(c.toLower) <|> char(c.toUpper) else char(c)
        if (desc.identDesc.caseSensitive) string(name)
        else name.foldLeft(pure(name))((p, c) => p <* caseChar(c)).label(name)
    }

    private val keywordMemo = concurrent.TrieMap.empty[String, Parsley[Unit]]
    override def softKeyword(name: String): Parsley[Unit] = keywordMemo.getOrElseUpdate(name, desc.identDesc.identLetter match {
        case Static(letter) => new Parsley(new singletons.Specific("keyword", name, letter, desc.identDesc.caseSensitive))
        case _ => attempt(caseString(name) *> notFollowedBy(identLetter).label(s"end of $name"))
    })

    private val operatorMemo = concurrent.TrieMap.empty[String, Parsley[Unit]]
    // TODO: I think operator and maxOp's behaviours should be merged, for consistency
    // This can be done with the `strings` combinator for now, we will want to optimise
    // it
    override def operator(name: String): Parsley[Unit] = operatorMemo.getOrElseUpdate(name, desc.opLetter match {
        case Static(letter) => new Parsley(new singletons.Specific("operator", name, letter, true))
        case _ => attempt(string(name) *> notFollowedBy(opLetter).label(s"end of $name"))
    })
    override def maxOp(name: String): Parsley[Unit] = new Parsley(new singletons.MaxOp(name, desc.operators))
}

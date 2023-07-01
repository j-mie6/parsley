/* SPDX-FileCopyrightText: © 2023 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debugger.internal

import scala.collection.mutable

import parsley.internal.deepembedding.frontend.LazyParsley
import parsley.internal.deepembedding.frontend.debugger.Debugged

// An object with a single public apply method that allows the renaming of a
// debugged  parser's name in order to increase the clarity of the debugger's
// results. By default, this does nothing other than invoke the name translation
// utility that is used to make symbolic operators.
// It also requires user interaction, as it needs to collect information from the
// classes or objects that define their parser.
private [parsley] object Rename {
    // This will attempt to match parsers referentially.
    // This also assumes that all parsers are present (lazy or otherwise) before debugging.
    // Populating this map is only possible if the platform contains some form of implementation for
    // parsley.debugger.utils.collectNames, which attempts to collect the name-in-code for a given
    // reference to a parser.
    lazy private val collected: mutable.Map[LazyParsley[_], String] = new mutable.HashMap()

    // This method attempts the renaming of a parser.
    def apply(optName: Option[String], p: LazyParsley[_]): String = {
        val defaultName = partial(p)

        val extracted = p match {
            case dbg: Debugged[_] => dbg.origin
            case _                => p
        }

        // This renames the parser if it is present, otherwise gives the default name found earlier.
        optName match {
            case Some(name) => name
            case None       => collected.getOrElse(extracted, defaultName)
        }
    }

    // Perform the first step of renaming, a partial rename where only the type name is exposed.
    def partial(p: LazyParsley[_]): String =
        translate(p match {
            case dbg: Debugged[_] => dbg.origin.prettyName
            case _ => p.prettyName
        })

    private [parsley] def addNames(names: Map[LazyParsley[_], String]): Unit =
        collected.addAll(names)

    // Translation table for Scala operator names.
    private [this] lazy val operatorTable: Map[String, Char] = Map(
        ("times", '*'),
        ("percent", '%'),
        ("div", '/'),
        ("plus", '+'),
        ("minus", '-'),
        ("colon", ':'),
        ("less", '<'),
        ("greater", '>'),
        ("eq", '='),
        ("bang", '!'),
        ("amp", '&'),
        ("up", '^'),
        ("bar", '|'),
        ("tilde", '~')
    )

    // Translate a fully-qualified class name into something more human-readable.
    private [this] def translate(name: String): String = {
        val lastDot = name.lastIndexOf(".")
        val uName =
            if (lastDot == -1) name
            else name.drop(lastDot + 1)

        if (uName.contains('$')) {
            uName.split('$')
                .map((seg: String) => operatorTable.get(seg).map(c => s"$c").getOrElse(seg))
                .mkString
        } else {
            uName
        }
    }
}

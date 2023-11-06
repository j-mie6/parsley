/*
 * Copyright (c) 2020, Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors
 *    may be used to endorse or promote products derived from this software without
 *    specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
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

    // Compatibility for Scala 2.12.
    implicit class MapAddAll[K, V](mutMap: mutable.Map[K, V]) {
        def addAllFrom(collection: Map[K, V]): Unit =
            collection.foreach { case (k, v) => mutMap(k) = v }

        def addAllFrom(collection: Iterable[(K, V)]): Unit =
            collection.foreach { case (k, v) => mutMap(k) = v }
    }

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
        collected.addAllFrom(names)

    private [parsley] def addName(par: LazyParsley[_], name: String): Unit = {
        val _ = collected.put(par, name)
        () // XXX: Map.put returns an option which needs to be discarded.
    }

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

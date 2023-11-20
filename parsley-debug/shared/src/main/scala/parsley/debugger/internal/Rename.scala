/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debugger.internal

import scala.collection.mutable

import org.typelevel.scalaccompat.annotation.unused
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

        // This is more a user-facing utility.
        // $COVERAGE-OFF$
        def addAllFrom(collection: Iterable[(K, V)]): Unit =
            collection.foreach { case (k, v) => mutMap(k) = v }
        // $COVERAGE-ON$
    }

    // This method attempts the renaming of a parser.
    def apply(optName: Option[String], p: LazyParsley[_]): String = {
        val defaultName = p match {
            case dbg: Debugged[_] => dbg.origin.prettyName
            // $COVERAGE-OFF$
            case _ => p.prettyName // Ideally this case isn't hit at all.
            // $COVERAGE-ON$
        }

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
        p match {
            // $COVERAGE-OFF$
            case dbg: Debugged[_] => dbg.origin.prettyName // Ideally this case isn't hit at all.
            // $COVERAGE-ON$
            case _ => p.prettyName
        }

    private [parsley] def addNames(names: Map[LazyParsley[_], String]): Unit =
        collected.addAllFrom(names)

    private [parsley] def addName(par: LazyParsley[_], name: String): Unit = {
        val _ = collected.put(par, name): @unused
    }
}

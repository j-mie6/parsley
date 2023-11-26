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
// results. By default, this does nothing other than get the prettified names of
// a parser's class defined within.
// It also requires user interaction, as it needs to collect information from the
// classes or objects that define their parser.
private [parsley] object Rename {
    // This will attempt to match parsers referentially.
    // This also assumes that all parsers are present (lazy or otherwise) before debugging.
    // Populating this map is only possible if the platform contains some form of implementation for
    // parsley.debugger.util.Collector.names, which attempts to collect the name-in-code for a given
    // reference to a parser.
    // Alternatively, users can use the `named` combinator from the debugger to weakly assign names
    // to their parsers, or directly invoke Collector.assignName to strongly assign names to parsers.
    lazy private val collected: mutable.Map[LazyParsley[_], String] = new mutable.HashMap()

    // Compatibility for Scala 2.12, and for convenience within the debugger for things to do with maps.
    private [debugger] implicit class MapAddAll[K, V](mutMap: mutable.Map[K, V]) {
        def addAllFrom(collection: Map[K, V]): Unit =
            collection.foreach { case (k, v) => mutMap(k) = v }
    }

    // This method attempts the renaming of a parser, even if at least partially.
    def apply(optName: Option[String], p: LazyParsley[_]): String = {
        val defaultName = partial(p)

        val extracted = p match {
            case dbg: Debugged[_] => dbg.origin
            case _                => p
        }

        // This renames the parser if it is present, otherwise gives the default name found earlier.
        optName.getOrElse(collected.getOrElse(extracted, defaultName))
    }

    // Perform the first step of renaming, a partial rename where only the type name is exposed.
    @inline def partial(p: LazyParsley[_]): String =
        p match {
            // $COVERAGE-OFF$
            case dbg: Debugged[_] => dbg.origin.prettyName // Ideally this case isn't hit at all.
            case _ => p.prettyName
            // $COVERAGE-ON$
        }

    private [parsley] def addNames(names: Map[LazyParsley[_], String]): Unit =
        collected.addAllFrom(names)

    // $COVERAGE-OFF$
    private [parsley] def addNames(names: Iterable[(LazyParsley[_], String)]): Unit =
        names.foreach { case (k, v) => collected(k) = v }
    // $COVERAGE-ON$

    private [parsley] def addName(par: LazyParsley[_], name: String): Unit = {
        val _ = collected.put(par, name): @unused
    }
}

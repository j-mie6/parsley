/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debugger.util

import parsley.Parsley
import parsley.debugger.internal.Rename
import parsley.token.Lexer

import parsley.internal.deepembedding.frontend.LazyParsley

/** Attempt to collect all the fields in a class or object that contain a
  * parser of type [[parsley.Parsley]], or from a [[parsley.token.Lexer]].
  *
  * This information is used later in the debug tree-building process to rename certain parsers
  * so that they do not end up being named things like "packageanon".
  *
  * You only need to run this once per parser-holding object.
  *
  * @since 4.5.0
  */
object Collector {
    /** Collect names of parsers from an object. */
    def names(obj: Any): Unit = {
        collectDefault() // Runs only once, ever, for a program execution.
        Rename.addNames(XCollector.collectNames(obj))
    }

    /** Collect names of parsers from a [[parsley.token.Lexer]]. */
    def lexer(lexer: Lexer): Unit = {
        collectDefault()
        Rename.addNames(XCollector.collectLexer(lexer))
    }

    // $COVERAGE-OFF$
    /** Manually add a name for a parser by reference.
      *
      * Can also be used if a more informative name for a parser is wanted.
      * In this case, use this method after using [[names]] or [[lexer]] to override the automatically
      * collected / found name.
      *
      * @note Names assigned using this will take precedence over names assigned using [[parsley.debugger.combinator.named]].
      */
    def assignName(par: Parsley[_], name: String): Unit =
        Rename.addName(par.internal, name)

    /** Does the implementation of the collector for the current Scala platform actually work in
      * automatically finding parsers in objects and getting their field names as written in your
      * parser code?
      *
      * @note Manually named parsers using [[assignName]] or [[parsley.debugger.combinator.named]]
      *       will still work regardless if the platform is supported or not.
      */
    @inline def isSupported: Boolean =
        XCollector.supported

    /** Collect the names of Parsley's various default singleton parsers. */
    private var defaultCollected: Boolean = false
    private def collectDefault(): Unit =
        if (isSupported) {
                this.synchronized {
                if (!defaultCollected) {
                    defaultCollected = true

                    names(parsley.character)
                    names(parsley.combinator)
                    names(parsley.Parsley)
                    names(parsley.position)
                }
            }
        }
    // $COVERAGE-ON$
}

/** A representation of the current implementation that [[Collector]] uses in order to
  * actually collect the names of parsers. One of these will need to be implemented under the name
  * `XCollector` under `parsley.debugger.util` for each different Scala runtime.
  *
  * @note This is an internal detail, so users do not have to interact with this if not necessary.
  */
// $COVERAGE-OFF$
abstract class CollectorImpl private [parsley] () {
    /** Collect names of parsers from an object. */
    def collectNames(obj: Any): Map[LazyParsley[_], String]

    /** Collect names of parsers from a [[parsley.token.Lexer]]. */
    def collectLexer(lexer: Lexer): Map[LazyParsley[_], String]

    /** Does the current platform's [[CollectorImpl]] actually get parsers from objects? */
    val supported: Boolean

    // Try grabbing a parser from a LazyParsley or Parsley instance.
    // XXX: Doing a direct type test with match will cause Parsley objects to be instantiated.
    protected def tryExtract(p: Any): LazyParsley[_] = {
        try {
            p.asInstanceOf[LazyParsley[_]]
        } catch {
            case _: ClassCastException => p.asInstanceOf[Parsley[_]].internal
        }
    }
}
// $COVERAGE-ON$

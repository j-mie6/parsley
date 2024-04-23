/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debugger.util

import scala.annotation.nowarn

import parsley.Parsley
import parsley.debugger.internal.Renamer
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
    /** Collect names of parsers from an object.
      *
      * @note For Scala 3 on the JVM, it is advised that all parsers in objects being introspected are
      *       marked `public`, as otherwise, a semi-deprecated call to `setAccessible` will be called on
      *       a private object, which may be restricted or removed in a future version of the JVM. It may
      *       be advised to manually name one's parsers (to be debugged) using [[assignName]] or
      *       [[parsley.debugger.combinator.named]] if that warning is not desirable.
      */
    @deprecated("The functionality of this class has been subsumed by the `parsley.debuggable` annotation", "5.0.0-M7")
    def names(obj: Any): Unit = {
        collectDefault() // Runs only once, ever, for a program execution.
        Renamer.addNames(XCollector.collectNames(obj))
    }

    def registerNames(names: Map[Parsley[_], String]): Unit = {
        Renamer.addNames(names.map {
            case (k, v) => k.internal -> v
        })
    }

    /** Collect names of parsers from a [[parsley.token.Lexer]].
      *
      * @note For Scala 3 on the JVM, this may trigger a warning about `setAccessible` for private members
      *       being deprecated.
      * @see [[names]] for more information regarding the warning.
      */
    //@deprecated("This functionality has been absorbed into parsley itself", "5.0.0-M7")
    def lexer(lexer: Lexer): Unit = {
        collectDefault()
        Renamer.addNames(XCollector.collectLexer(lexer))
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
    def assignName(par: Parsley[_], name: String): Unit = Renamer.addName(par.internal, name)

    /** Does the implementation of the collector for the current Scala platform actually work in
      * automatically finding parsers in objects and getting their field names as written in your
      * parser code?
      *
      * @note Manually named parsers using [[assignName]] or [[parsley.debugger.combinator.named]]
      *       will still work regardless if the platform is supported or not.
      */
    @inline def isSupported: Boolean = XCollector.supported

    /** Collect the names of Parsley's various default singleton parsers. */
    private var defaultCollected: Boolean = false
    private def collectDefault(): Unit = if (isSupported) {
        this.synchronized {
            if (!defaultCollected) {
                defaultCollected = true

                names(parsley.character): @nowarn
                names(parsley.combinator): @nowarn
                names(parsley.Parsley): @nowarn
                names(parsley.position): @nowarn
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
private [parsley] abstract class CollectorImpl {
    /** Collect names of parsers from an object. */
    def collectNames(obj: Any): Map[LazyParsley[_], String]

    /** Collect names of parsers from a [[parsley.token.Lexer]]. */
    def collectLexer(lexer: Lexer): Map[LazyParsley[_], String]

    /** Does the current platform's [[CollectorImpl]] actually get parsers from objects? */
    val supported: Boolean

    // Try grabbing a parser from a LazyParsley or Parsley instance.
    // XXX: Doing a direct type test with match will cause Parsley objects to be instantiated.
    // XXX: Using a match-case expression without @unchecked causes an error in CI as these matches are not exhaustive.
    protected def tryExtract(par: Any): LazyParsley[_] = (par: @unchecked) match {
        case l: LazyParsley[_] => l
        case p: Parsley[_]     => p.internal
    }

    // All of these objects inside a lexer are exposed, so are easy to collect parser names from.
    // The rest will need to be handled by reflection.
    // If any public objects are added to Lexer, please add them to this list.
    @inline protected final def lexerObjects(lexer: Lexer): List[Any] = List(
        lexer,
        lexer.space,
        lexer.lexeme,
        lexer.lexeme.integer,
        lexer.lexeme.natural,
        lexer.lexeme.real,
        lexer.lexeme.unsignedCombined,
        lexer.lexeme.signedCombined,
        lexer.lexeme.character,
        lexer.lexeme.string,
        lexer.lexeme.multiString,
        lexer.lexeme.rawString,
        lexer.lexeme.rawMultiString,
        lexer.lexeme.names,
        lexer.lexeme.symbol,
        lexer.nonlexeme,
        lexer.nonlexeme.integer,
        lexer.nonlexeme.natural,
        lexer.nonlexeme.real,
        lexer.nonlexeme.unsignedCombined,
        lexer.nonlexeme.signedCombined,
        lexer.nonlexeme.character,
        lexer.nonlexeme.string,
        lexer.nonlexeme.multiString,
        lexer.nonlexeme.rawString,
        lexer.nonlexeme.rawMultiString,
        lexer.nonlexeme.names,
        lexer.nonlexeme.symbol,
    )
}
// $COVERAGE-ON$

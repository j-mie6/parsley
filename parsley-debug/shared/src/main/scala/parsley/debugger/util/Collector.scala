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

package parsley.debugger.util

import parsley.Parsley
import parsley.token.Lexer

import parsley.internal.deepembedding.frontend.LazyParsley

/** Attempt to collect all the fields in a class or object that contain a
  * parser of type [[parsley.Parsley]], or from a [[parsley.token.Lexer]].
  *
  * This information is used later in the debug tree-building process to rename certain parsers
  * so that they do not end up being named things like "packageanon".
  *
  * You only need to run this once per parser-holding object.
  */
object Collector {
    /** Collect names of parsers from an object. */
    def names(obj: Any): Unit = {
        collectDefault() // Runs only once, ever, for a program execution.
        Rename.addNames(XCollector.collectNames(obj))
    }

    /** Collect names of parsers from a [[parsley.token.Lexer]]. */
    def lexer(lexer: Lexer): Unit =
        Rename.addNames(XCollector.collectLexer(lexer))

    /** Manually add a name for a parser by reference.
      *
      * Can also be used if a more informative name for a parser is wanted.
      * In this case, use this method after using [[names]] or [[lexer]] to override the automatically
      * collected / found name.
      */
    def assignName(par: Parsley[_], name: String): Unit =
        Rename.addName(par.internal, name)

    /** Does the implementation of the collector for the current Scala platform actually work in
      * automatically finding parsers in objects and getting their field names as written in your
      * parser code?
      */
    def isSupported: Boolean =
        XCollector.supported

    /** Collect the names of Parsley's various default singleton parsers. */
    private var defaultCollected: Boolean = false
    private def collectDefault(): Unit = this.synchronized {
        if (!defaultCollected) {
            defaultCollected = true

            names(parsley.character)
            names(parsley.combinator)
            names(parsley.Parsley)
            names(parsley.position)
        }
    }
}

/** A representation of the current implementation that [[Collector]] uses in order to
  * actually collect the names of parsers. One of these will need to be implemented under the name
  * `XCollector` under `parsley.debugger.util` for each different Scala runtime.
  */
abstract class CollectorImpl private [parsley] () {
    /** Collect names of parsers from an object. */
    def collectNames(obj: Any): Map[LazyParsley[_], String]

    /** Collect names of parsers from a [[parsley.token.Lexer]]. */
    def collectLexer(lexer: Lexer): Map[LazyParsley[_], String]

    /** Does the current platform's [[CollectorImpl]] actually get parsers from objects? */
    val supported: Boolean

    // Try grabbing a parser from a LazyParsley or Parsley instance.
    protected def tryExtract(p: Any): LazyParsley[_] = {
        try {
            p.asInstanceOf[LazyParsley[_]]
        } catch {
            case _: ClassCastException => p.asInstanceOf[Parsley[_]].internal
        }
    }
}

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

import scala.annotation.nowarn
import scala.collection.mutable
import scala.reflect.runtime.{universe => ru}

import parsley.Parsley
import parsley.token.Lexer

import parsley.internal.deepembedding.frontend.LazyParsley

private [parsley] object XCollector extends CollectorImpl {
    // True, it works, but it is on the whims of reflection functioning as expected.
    override val supported: Boolean = true

    // There should not be too many differences in the public API between 2.12 and 2.13's reflection
    // packages. However, results may vary. Scala 3 however, is a wild-west of compatibility.
    // XXX: @nowarn is required as the Runtime Universe's MethodSymbol type is erased at runtime,
    //      though it should be a safe type comparison if the version of scala-reflect remains
    //      constant between compile-time and runtime.
    // XXX: This works for Scala 2, but it is not guaranteed to work at all for Scala 3.
    @nowarn def collectNames(obj: Any): Map[LazyParsley[_], String] = {
        val accumulator: mutable.HashMap[LazyParsley[_], String] = new mutable.HashMap()

        val mirror = scala.reflect.runtime.currentMirror
        val objRefl = mirror.reflect(obj)
        val objClass = mirror.classSymbol(obj.getClass)

        // XXX: Does not work with def-defined custom parsers due to new references of
        //      parsers being created with each invocation.
        val getters = objClass.toType.members.collect {
            case mem: ru.MethodSymbol if mem.isGetter => mem
        }

        for (getter <- getters) {
            val getterRefl = objRefl.reflectMethod(getter)
            val parser = getterRefl()

            parser match {
                case _: LazyParsley[_] | _: Parsley[_] =>
                    val name = getter.name.toString
                    accumulator.put(tryExtract(parser), name)
                case _ =>
                    () // Don't actually do anything.
            }
        }

        accumulator.toMap
    }

    // XXX: See collectNames' hack (XXX) message for more information.
    @nowarn def collectLexer(lexer: Lexer): Map[LazyParsley[_], String] = {
        val accumulator: mutable.HashMap[LazyParsley[_], String] = new mutable.HashMap()

        val lexerObjects = List(
            lexer,
            lexer.space,
            lexer.lexeme,
            lexer.lexeme.names,
            lexer.lexeme.text,
            lexer.lexeme.enclosing,
            lexer.lexeme.separators,
            lexer.lexeme.symbol,
            lexer.lexeme.numeric,
            lexer.nonlexeme,
            lexer.nonlexeme.names,
            lexer.nonlexeme.numeric,
            lexer.nonlexeme.symbol,
            lexer.nonlexeme.text
        )

        lexerObjects.foreach(obj => accumulator.addAll(collectNames(obj)))

        val mirror = scala.reflect.runtime.currentMirror
        val reflPairs = List(
            lexer.lexeme.text,
            lexer.lexeme.numeric,
            lexer.nonlexeme.text,
            lexer.nonlexeme.numeric
        ).map(obj => (mirror.classSymbol(obj.getClass), mirror.reflect(obj)))

        for ((clazz, lexRefl) <- reflPairs) {
            val getters = clazz.toType.members.collect {
                case mem: ru.MethodSymbol if mem.isGetter => mem
            }

            for (getter <- getters) {
                val innerRefl = lexRefl.reflectMethod(getter)
                accumulator.addAll(collectNames(innerRefl()))
            }
        }

        accumulator.toMap
    }
}

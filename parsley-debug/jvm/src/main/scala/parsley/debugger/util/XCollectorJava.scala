/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debugger.util

import scala.collection.mutable
import scala.util.{Success, Try}

import parsley.debugger.internal.Rename.MapAddAll
import parsley.token.Lexer

import parsley.internal.deepembedding.frontend.LazyParsley
import parsley.internal.deepembedding.frontend.debugger.helper.{lazyParsleyExample, parsleyExample}

// Java-reflection powered XCollector. For backup purposes. Not meant to be used directly, but inherited from.
// The setAccessible methods on private members is a deprecated action, so proceed with caution.
// $COVERAGE-OFF$
private [parsley] abstract class XCollectorJava extends CollectorImpl {
    override val supported: Boolean = true

    private def getParsers(obj: Any): Try[Map[LazyParsley[_], String]] = {
        val accumulator: mutable.HashMap[LazyParsley[_], String] = new mutable.HashMap()

        Try {
            // FIXME: Type matching by name is very brittle...
            val parserGetters = obj.getClass.getDeclaredMethods.filter { mth =>
                // lazyParsleyExample is an anonymous class, so we have to get the superclass name for LazyParsley.
                Seq(parsleyExample.getClass.getName, lazyParsleyExample.getClass.getSuperclass.getName)
                    .map(Class.forName).contains(mth.getReturnType) && mth.getParameterCount == 0
            }

            parserGetters.foreach(_.setAccessible(true)) // FIXME: This is deprecated API and may be removed in a later Java version!
            parserGetters.foreach { mth => accumulator(tryExtract(mth.invoke(obj))) = mth.getName }

            accumulator.toMap
        }.orElse(Success(accumulator.toMap))
    }

    override def collectNames(obj: Any): Map[LazyParsley[_], String] = getParsers(obj).getOrElse(Map.empty)

    override def collectLexer(lexer: Lexer): Map[LazyParsley[_], String] = {
        val accumulator: mutable.HashMap[LazyParsley[_], String] = new mutable.HashMap()

        Try {
            // Safe lexer objects need only one reflection step.
            safeLexerObjects(lexer).foreach(obj => accumulator.addAllFrom(collectNames(obj)))

            // For these "unsafe" objects, we need to reflect them twice.
            unsafeLexerObjects(lexer).foreach { uob =>
                val getters = uob.getClass.getDeclaredMethods.filter(_.getParameterCount == 0)
                getters.foreach(_.setAccessible(true))
                getters.foreach(get => accumulator.addAllFrom(collectNames(get.invoke(uob))))
            }

            accumulator.toMap
        }.orElse(Success(accumulator.toMap))
    }.getOrElse(Map.empty)
}
// $COVERAGE-ON$

/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debugger.util

import scala.collection.mutable
import scala.reflect.runtime.{universe => ru}

import parsley.Parsley
import parsley.token.Lexer

import parsley.internal.deepembedding.frontend.LazyParsley

private [parsley] object XCollector extends CollectorImpl {
  // There should not be too many differences in the public API between 2.12 and 2.13's reflection
  // packages. However, results may vary. Scala 3 however, is a wild-west of compatibility.
  def collectNames(obj: Any): Map[LazyParsley[_], String] = {
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

  def collectLexer(lexer: Lexer): Map[LazyParsley[_], String] = {
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

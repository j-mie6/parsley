/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.frontend

import parsley.debugger.objects.TransientDebugTree

import parsley.internal.deepembedding.frontend

import scala.collection.mutable

package object debugger {
  private [parsley] def traverseDown[A](parser: LazyParsley[A])(implicit seen: mutable.Set[LazyParsley[_]]): (TransientDebugTree, LazyParsley[A]) =
  // This stops recursive parsers from causing an infinite recursion.
    if (seen.contains(parser)) {
      (TransientDebugTree(name = "RECUR"), parser)
    } else {
      // Without this, we could potentially have infinite recursion from lazy-initialised parsers.
      seen.add(parser)

      // This tree will be populated as the parser is run.
      // The name of the parser will be the class name of the parser, translated into
      // something more human-friendly.
      val currentTree = TransientDebugTree(name = translate(parser.getClass.getSimpleName))

      // Function is buried in the frontend package to facilitate access to the GeneralisedEmbedding
      // abstract classes and their getters.
      val children = getChildren(parser).map(traverseDown(_))
      currentTree.children = currentTree.children ::: children.map(_._1)

      (currentTree, parser)
    }

  // Translation table for Scala operator names.
  private lazy val operatorTable: Map[String, Char] = Map(
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
  private def translate(name: String): String =
    if (name.contains('$')) {
      name.split('$').map(operatorTable.getOrElse(_, "")).mkString
    } else {
      name
    }

  // Attempt to retrieve the child parsers.
  private def getChildren(parser: LazyParsley[_]): List[LazyParsley[_]] =
    parser match {
      case p: frontend.Unary[_, _]         => List(p.parser)
      case p: frontend.Binary[_, _, _]     => List(p.leftParser, p.rightParser)
      case p: frontend.Ternary[_, _, _, _] => List(p.firstParser, p.secondParser, p.thirdParser)
      case p: frontend.<|>[_]              => List(p.leftParser, p.rightParser)
      case p: frontend.ChainPre[_]         => List(p.itemParser, p.opParser)
      case _ =>
        // This catches all atomic parsers (e.g. satisfy parsers).
        Nil
    }
}

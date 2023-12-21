/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debugger.internal

import scala.collection.immutable.ListMap
import scala.collection.mutable

import parsley.debugger.{DebugTree, ParseAttempt}
import parsley.debugger.util.XMap

/** A mutable implementation of [[DebugTree]], used when constructing the tree as a parser is
  * running.
  *
  * When viewing / analysing the parse tree, it is highly advised to call
  * [[TransientDebugTree#freeze]] to obtain a frozen, immutable version of the debug tree.
  *
  * @param name Name of parser.
  * @param parse What attempts to parse have been made?
  * @param children This debug tree node's children.
  */
private [parsley] case class TransientDebugTree(
    var name: String = "",
    var internal: String = "",
    fullInput: String,
    var parse: Option[ParseAttempt] = None,
    var cNumber: Option[Long] = None,
    children: mutable.Map[String, TransientDebugTree] = new mutable.LinkedHashMap()
) extends DebugTree {
    // These are user-facing, and will depend heavily on what the parser looks like.
    // $COVERAGE-OFF$
    override def parserName: String = name

    override def internalName: String = internal

    override def childNumber: Option[Long] = cNumber

    // The pair stores the input the parser attempted to parse and its success.
    override def parseResults: Option[ParseAttempt] = parse

    override val nodeChildren: Map[String, DebugTree] = new XMap[String, DebugTree] {
        // We'll use a copy-on-write methodology for this.
        override def removed(key: String): Map[String, DebugTree] =
            children.foldLeft(ListMap.empty[String, DebugTree])((acc, p) => acc + p) - key

        // See above.
        override def updated[V1 >: DebugTree](key: String, value: V1): Map[String, V1] =
            children.foldLeft(ListMap.empty[String, V1])((acc, p) => acc + p) + ((key, value))

        // For get, size and iterator, we'll just use the mutable map.
        override def get(key: String): Option[DebugTree] = children.get(key)

        override def iterator: Iterator[(String, DebugTree)] = children.iterator

        override def size: Int = children.size
    }

    // Factors out inputs or results for parsers with children.
    private type Augment  = (Long, (Int, Int))
    private var augmentId = 0L
    private val augments  = mutable.ListBuffer.empty[Augment]

    private [parsley] def augmentInput(startIndex: Int, endIndex: Int): Long = {
        augmentId += 1L

        val uuid = augmentId
        augments.append((uuid, (startIndex, endIndex)))

        uuid
    }

    private [parsley] def applyInputAugments(): TransientDebugTree = {
        parse = parse.map { p =>
            // Augments are single-use.
            val ua = augments.toList
            augments.clear()

            def basis(int: Int): Int = int - p.fromOffset

            p.copy(
                inp = ua.foldRight(p.rawInput) { case ((aid, (ast, aen)), st) => st.slice(0, basis(ast)) + s"{$aid}" + st.drop(basis(aen)) },
            )
        }

        this
    }
}

/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debugger.internal

import scala.collection.immutable.ListMap
import scala.collection.mutable

import parsley.debugger.{DebugTree, ParseAttempt}

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

    override val nodeChildren: Map[String, DebugTree] =
        children.foldLeft[ListMap[String, DebugTree]](new ListMap())(_ + _)

    // Factors out inputs or results for parsers with children.
    private type Augment = (Long, (Int, Int))
    private var augmentId: Long                       = 0L
    private val augments: mutable.ListBuffer[Augment] = mutable.ListBuffer()

    private [parsley] def augmentInput(startIndex: Int, endIndex: Int): Long = {
        augmentId = augmentId + 1

        val uuid = augmentId
        augments.append((uuid, (startIndex, endIndex)))

        uuid
    }

    private [parsley] def applyInputAugments(): TransientDebugTree = {
        parse = parse.map { p =>
            // Augments are single-use.
            val ua = augments.toList
            augments.clear()

            def basis(int: => Int): Int =
                int - p.fromOffset

            p.copy(
                inp = ua.foldRight(p.rawInput) { case ((aid, (ast, aen)), st) => st.slice(0, basis(ast)) + s"{$aid}" + st.drop(basis(aen)) },
            )
        }

        this
    }
}

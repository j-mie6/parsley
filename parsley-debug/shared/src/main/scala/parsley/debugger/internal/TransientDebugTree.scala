/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debugger.internal

import scala.collection.immutable.ListMap
import scala.collection.mutable

import parsley.debugger.{DebugTree, ParseAttempt}
import parsley.debugger.internal.ToXMap._ // scalastyle:ignore underscore.import

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
    override def parserName: String = name

    override def internalName: String = internal

    override def childNumber: Option[Long] = cNumber

    // The pair stores the input the parser attempted to parse and its success.
    override def parseResults: Option[ParseAttempt] = parse

    override val nodeChildren: Map[String, DebugTree] = new XMap[String, DebugTree] {
        private val conv: ToXMap[Map] = implicitly[ToXMap[Map]]

        // We'll use a copy-on-write methodology for this.
        override def removed(key: String): Map[String, DebugTree] =
            conv.toXMap(children.foldLeft(ListMap[String, DebugTree]())((acc, p) => acc + p)).removed(key)

        // See above.
        override def updated[V1 >: DebugTree](key: String, value: V1): Map[String, V1] =
            conv.toXMap(children.foldLeft(ListMap[String, DebugTree]())((acc, p) => acc + p)).updated(key, value)

        // For get, size and iterator, we'll just use the mutable map.
        override def get(key: String): Option[DebugTree] = children.get(key)

        override def iterator: Iterator[(String, DebugTree)] = children.iterator

        override def size: Int = children.size
    }

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

            p.copy(rawInput = ua.foldRight(p.rawInput) { case ((aid, (ast, aen)), st) =>
                st.slice(0, basis(ast)) + s"{$aid}" + st.drop(basis(aen))
            })
        }

        this
    }

    /** Freeze the current debug tree into an immutable copy.
      *
      * It is highly advised to do this before analysing the tree.
      *
      * @return An anonymous immutable copy of this tree.
      */
    def freeze: DebugTree = {
        // Freeze any mutable values by copying them.
        // Also freeze all child trees because we don't want to have to manually freeze the whole tree.
        val immName = name
        val immInternal = internal
        val immCN = cNumber
        val immParse = parse
        val immInp = fullInput
        val immChildren = children.map {
            case (n, t: TransientDebugTree) => (n, t.freeze)
        }.toMap

        // There doesn't seem to be much of a point in making a whole new class for immutable trees
        // as pattern-matching is less of a worry.
        new DebugTree {
            override def parserName: String = immName

            override def internalName: String = immInternal

            override def childNumber: Option[Long] = immCN

            override def parseResults: Option[ParseAttempt] = immParse

            override def nodeChildren: Map[String, DebugTree] = immChildren

            override def fullInput: String = immInp
        }
    }
}

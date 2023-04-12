/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debugger.objects

import parsley.internal.deepembedding.frontend.LazyParsley

import scala.collection.mutable

/** The tree representing a parser's parse tree.
  *
  * Initially unpopulated, it will be populated with information regarding the parser, such as
  * what it is (if it is a primitive or a command like [[parsley.Parsley.foldLeft]] as the parser
  * itself runs on some input.
  *
  * Any tree node will store the input it has parsed (or attempted to parse) as well as its
  * success state.
  */
sealed trait DebugTree {
  /** What is the name of the parser that made this node. */
  def parserName: String

  /** A map of parent debug trees to parse input and success pairs. */
  def parseResults: List[(String, Boolean)]

  /** What are the child debug nodes for this node? */
  def nodeChildren: Map[String, DebugTree]
}

/** A mutable implementation of [[DebugTree]], used when constructing the tree as a parser is
  * running.
  *
  * When viewing / analysing the parse tree, it is highly advised to call
  * [[TransientDebugTree#freeze]] to obtain a frozen, immutable version of the debug tree.
  *
  * @param name Name of parser.
  * @param parses What attempts to parse have been made?
  * @param children This debug tree node's children.
  */
case class TransientDebugTree(
  var name: String = "",
  parses: mutable.ListBuffer[(String, Boolean)] = new mutable.ListBuffer(),
  children: mutable.Map[String, TransientDebugTree] = new mutable.LinkedHashMap()
) extends DebugTree {
  override def parserName: String = name

  // The pair stores the input the parser attempted to parse and its success.
  override def parseResults: List[(String, Boolean)] = parses.toList

  override def nodeChildren: Map[String, DebugTree] = children.toMap

  /** Add the next child to this tree.
    * This can be visualised by adding it to the right of the rightmost child if it exists.
    *
    * @param tree Tree to add as a child.
    */
  @deprecated def addChild(tree: TransientDebugTree): Unit =
    children.addOne(tree.name, tree)

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

    val immParses = parses.toList

    val immChildren = children.map {
      case (n, t: TransientDebugTree) => (n, t.freeze)
      case other                      => other
    }.toMap

    // There doesn't seem to be much of a point in making a whole new class for immutable trees
    // as pattern-matching is less of a worry.
    new DebugTree {
      override def parserName: String = immName

      override def parseResults: List[(String, Boolean)] = immParses

      override def nodeChildren: Map[String, DebugTree] = immChildren
    }
  }
}

// Helper class for reconstructing a debug tree.
private [parsley] case class DebugTreeBuilder(
  node: TransientDebugTree,
  bChildren: Map[LazyParsley[Any], DebugTreeBuilder] = Map.empty,
) {
  def addNode(path: List[LazyParsley[Any]], node: TransientDebugTree): DebugTreeBuilder =
    path match {
      case Nil      => DebugTreeBuilder(node, Map.empty)
      case p :: ps  =>
        // Pre: The path to this node must fully exist.
        // Tip: Add the shortest paths first!
        val child = this.bChildren.getOrElse(p, DebugTreeBuilder(node))
        DebugTreeBuilder(this.node, this.bChildren.+((p, child.addNode(ps, node))))
    }

  def reconstruct: TransientDebugTree = {
    node.children
      .addAll(
        bChildren.map { case (lp, cs) => (DebugContext.translate(lp.getClass.getTypeName), cs.reconstruct) }
      )

    node
  }
}

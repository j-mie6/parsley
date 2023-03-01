/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debugger.objects

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

  /** What is the input that was parsed / attempted to be parsed by the parser this node
    * represents?
    */
  def attemptedInput: String

  /** Was the parser that made this node successful? */
  def isSuccessful: Boolean

  /** What are the child debug nodes for this node? */
  def nodeChildren: List[_ <: DebugTree]
}

/** A mutable implementation of [[DebugTree]], used when constructing the tree as a parser is
  * running.
  *
  * When viewing / analysing the parse tree, it is highly advised to call
  * [[TransientDebugTree#freeze]] to obtain a frozen, immutable version of the debug tree.
  *
  * @param name Name of parser.
  * @param input Parser's given input.
  * @param successful Was the parser successful?
  * @param children This debug tree node's children.
  */
case class TransientDebugTree(
  var name: String = "",
  var input: String = "",
  var successful: Boolean = true,
  var children: List[_ <: DebugTree] = Nil
) extends DebugTree {
  override def parserName: String = name

  override def attemptedInput: String = input

  override def isSuccessful: Boolean = successful

  override def nodeChildren: List[_ <: DebugTree] = children

  /** Add the next child to this tree.
    * This can be visualised by adding it to the right of the rightmost child if it exists.
    *
    * @param tree Tree to add as a child.
    */
  def addChild(tree: TransientDebugTree): Unit =
    children = tree :: children

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
    val immInput = input
    val immSuccessful = successful
    val immChildren = children.map {
      case t: TransientDebugTree => t.freeze
      case other                 => other
    }

    // There doesn't seem to be much of a point in making a whole new class for immutable trees
    // as pattern-matching is less of a worry.
    new DebugTree {
      override def parserName: String = immName

      override def attemptedInput: String = immInput

      override def isSuccessful: Boolean = immSuccessful

      override def nodeChildren: List[_ <: DebugTree] = immChildren
    }
  }
}

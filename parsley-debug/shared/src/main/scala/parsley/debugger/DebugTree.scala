/* SPDX-FileCopyrightText: © 2023 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debugger

/** The tree representing a parser's parse tree.
  *
  * Initially unpopulated, it will be populated with information regarding the parser, such as
  * what it is (if it is a primitive such as [[parsley.internal.deepembedding.singletons.Pure]],
  * or a user-defined named parser if names are collected) as the parser itself runs on some input.
  *
  * Any tree node will store the input it has parsed (or attempted to parse) as well as its
  * success state as an optional [[ParseAttempt]] instance.
  *
  * Although this trait is unsealed, it is not useful to make a subtype of this trait, as this
  * trait's sole purpose is to provide safe methods into handling the frozen trees produced by
  * the debugger.
  */
trait DebugTree {
  /** The name of the parser that made this node. */
  def parserName: String

  /** The type name of the parser that formed this node. */
  def internalName: String

  /** Get the potential parse attempt recorded for this particular parser. */
  def parseResults: Option[ParseAttempt]

  /** What are the child debug nodes for this node?
    *
    * The map provided by the implementation should be a linked map in order to preserve the
    * order of child parser occurrences within each parser.
    *
    * Internally, child nodes are given an arbitrary numeric suffix to disambiguate them in the map
    * if multiple child nodes have the same parser name.
    *
    * Those internal names are not represented if checking [[parserName]].
    */
  def nodeChildren: Map[String, DebugTree]

  /** Get the full input that was attempted to be parsed by the debugged parser.
    *
    * This is the whole input, unaltered, even parts where the parser did not attempt to parse.
    */
  def fullInput: String

  override def toString: String =
    s"DebugTree { name: $parserName ($internalName), success: ${parseResults.exists(_.success)}, children: ${nodeChildren.keys} }"
}


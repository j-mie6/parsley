/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debugger

import scala.annotation.tailrec
import scala.collection.mutable

/** The tree representing a parser's parse tree.
  *
  * Initially unpopulated, it will be populated with information regarding the parser, such as
  * what it is (if it is a primitive such as [[parsley.internal.deepembedding.singletons.Pure]],
  * or a user-defined named parser if names are collected) as the parser itself runs on some input.
  *
  * Any tree node will store the input it has parsed (or attempted to parse) as well as its
  * success state as a list of pairs representing the parse attempts the parser has tried.
  *
  * The pairs consist of the input that was attempted to be parsed, and a boolean representing
  * if that particular parse was successful.
  *
  * Although this trait is unsealed, it is not useful to make a subtype of this trait, as this
  * trait's sole purpose is to provide safe methods into handling the frozen trees produced by
  * the debugger.
  */
trait DebugTree {
  /** What is the name of the parser that made this node. */
  def parserName: String

  /** A map of parent debug trees to parse input and success pairs. */
  def parseResults: List[(String, Boolean)]

  /** What are the child debug nodes for this node? */
  def nodeChildren: Map[String, DebugTree]

  override def toString: String =
    prettyPrint(PrettyPrintHelper(new StringBuilder, Vector.empty)).acc.dropRight(1).toString()

  // Internal pretty-printer method.
  private def prettyPrint(helper: PrettyPrintHelper): PrettyPrintHelper = {
    val results = parseResults.map(printParseAttempt).mkString(", ")
    helper.bury(s"[ $parserName ]: $results")
    printChildren(helper, nodeChildren.toList)
    helper
  }

  // Print a parse attempt in a human-readable way.
  private def printParseAttempt(attempt: (String, Boolean)): String =
    s"(\"${attempt._1}\", ${if (attempt._2) "Success" else "Failure"})"

  // Print all the children, remembering to add a blank indent for the last child.
  @tailrec private def printChildren
    ( helper: PrettyPrintHelper
    , children: List[(String, DebugTree)]
    ): Unit =
    children match {
      case (_, t) :: Nil =>
        helper.bury("|", withMark = false)
        t.prettyPrint(helper.addBlankIndent())
      case (_, t) :: xs  =>
        helper.bury("|", withMark = false)
        t.prettyPrint(helper.addIndent())
        printChildren(helper, xs)
      case Nil           => ()
    }
}

// Utility class for aiding in the toString method for debug trees.
private case class PrettyPrintHelper(acc: mutable.StringBuilder, indents: Vector[String]) {
  // Indent a string with the given indenting delimiters.
  def bury(str: String, withMark: Boolean = true): Unit = {
    val pretty = if (indents.isEmpty) str
                 else if (withMark) indents.init.mkString + "+-" + str
                      else indents.mkString + str

    acc.append(pretty + "\n")
  }

  // Add a new indent delimiter to the current helper instance.
  // The accumulator is shared between new instances.
  def addIndent(): PrettyPrintHelper =
    PrettyPrintHelper(acc, indents :+ "| ")

  def addBlankIndent(): PrettyPrintHelper =
    PrettyPrintHelper(acc, indents :+ "  ")
}

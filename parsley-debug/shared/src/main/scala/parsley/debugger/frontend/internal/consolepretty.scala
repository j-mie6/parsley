/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debugger.frontend.internal

import scala.annotation.tailrec
import scala.collection.mutable

import parsley.debugger.{DebugTree, ParseAttempt}

object consolepretty {
    // Utility class for aiding in the toString method for debug trees.
    private [frontend] case class PrettyPrintHelper(acc: mutable.StringBuilder, indents: Vector[String]) {
        // Indent a string with the given indenting delimiters.
        def bury(str: String, withMark: Boolean = true): Unit = {
            val pretty =
                if (indents.isEmpty) str
                else if (withMark) indents.init.mkString + "+-" + str
                else indents.mkString + str

            acc.append(pretty + "\n")
        }

        // Add a new indent delimiter to the current helper instance.
        // The accumulator is shared between new instances.
        def addIndent: PrettyPrintHelper = PrettyPrintHelper(acc, indents :+ "| ")

        // Adds a two-blank-space indent instead for the last child of a node.
        def addBlankIndent: PrettyPrintHelper = PrettyPrintHelper(acc, indents :+ "  ")
    }

    implicit final class TreePrinter(val dt: DebugTree) extends AnyVal {
        def pretty: String = {
            val acc = new mutable.StringBuilder
            prettyPrint(PrettyPrintHelper(acc, Vector.empty))
            acc.init.toString
        }

        private [TreePrinter] def prettyPrint(helper: PrettyPrintHelper): Unit = {
            val uname =
                if (dt.parserName != dt.internalName)
                    s"${dt.parserName} (${dt.internalName}${if (dt.childNumber.isDefined) s" (${dt.childNumber.get})" else ""})"
                else
                    s"${dt.internalName}${if (dt.childNumber.isDefined) s" (${dt.childNumber.get})" else ""}"
            val results = dt.parseResults.map(printParseAttempt).mkString

            helper.bury(s"[ $uname ]: $results")
            printChildren(helper, dt.nodeChildren.toList)
        }

        // Print a parse attempt in a human-readable way.
        private [TreePrinter] def printParseAttempt(attempt: ParseAttempt): String =
            s"(\"${attempt.rawInput}\" [${attempt.fromPos} -> ${attempt.toPos}], ${if (attempt.success)
                s"Success - [ ${attempt.result.get} ]"
                else "Failure"})"

        // Print all the children, remembering to add a blank indent for the last child.
        @tailrec private def printChildren(helper: PrettyPrintHelper, children: List[(String, DebugTree)]): Unit = children match {
            case (_, t) :: Nil =>
                helper.bury("|", withMark = false)
                t.prettyPrint(helper.addBlankIndent)
            case (_, t) :: xs =>
                helper.bury("|", withMark = false)
                t.prettyPrint(helper.addIndent)
                printChildren(helper, xs)
            case Nil =>
      }
  }
}

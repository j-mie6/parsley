/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debugger.internal

import scala.collection.mutable.ListBuffer

import parsley.debugger.ParseAttempt
import parsley.debugger.combinator.defaultRules

import parsley.internal.deepembedding.frontend.LazyParsley

// Class used to hold details about a parser being debugged.
// This is normally held as a value inside an implicit variable.
// Anything caught by the toStringRules will have a parse result of that type toString-ed for memory
// efficiency.
private [parsley] class DebugContext(val toStringRules: Seq[Any => Boolean] = defaultRules) {
    // Create a new dummy root of the tree that will act as filler for the rest of the tree to build
    // off of (as there is no "nil" representation for the tree... other than null, which should be
    // avoided in Scala wherever possible).
    private val dummyRoot: TransientDebugTree =
        TransientDebugTree("ROOT", "ROOT", "NIL")

    // Context's checkStack vanished.
    // Migrating it here for futureproofing.
    private var checkStack: ListBuffer[(Int, Int, Int)] =
        ListBuffer()

    def pushPos(offset: Int, line: Int, col: Int): Unit = {
        checkStack.prepend((offset, line, col))
    }

    def popPos(): (Int, Int, Int) = {
        checkStack.remove(0)
    }

    // Tracks where we are in the parser callstack.
    private var builderStack: ListBuffer[TransientDebugTree] =
        ListBuffer(dummyRoot)

    // Get the final DebugTree from this context.
    def getFinalTree: TransientDebugTree = {
        // The root tree exists only as a placeholder for the rest of the debug tree to build off of.
        // If it has no children, that means the debug tree was not built to begin with.
        // If it was multiple children, somehow the debugger has popped too many tree nodes off the stack.
        val ch = builderStack.head.children
        assert(!(ch.size < 1), s"The root tree has somehow lost its only child. (${ch.size})")
        assert(!(ch.size > 1), s"The root tree has somehow gained multiple children. (${ch.size})")

        // This should never fail.
        ch.collectFirst { case (_, x) => x }.get
    }

    // Add an attempt of parsing at the current stack point.
    def addParseAttempt(attempt: ParseAttempt): Unit =
        builderStack.head.parse = Some(attempt)

    // Reset this context back to zero.
    def reset(): Unit = {
        // Clear anything hanging off the dummy root.
        dummyRoot.children.clear()

        builderStack = ListBuffer(dummyRoot)
        checkStack   = ListBuffer()
    }

    // Unique parser IDs.
    private var uid: Long = -1L
    private def nextUid(): Long = {
        uid = uid + 1
        uid
    }

    // Push a new parser onto the parser callstack.
    def push(fullInput: String, parser: LazyParsley[_], optName: Option[String]): Unit = {
        val newTree = TransientDebugTree(fullInput = fullInput)
        newTree.name = Rename(optName, parser)
        newTree.internal = Rename.partial(parser)

        builderStack.head.children(newTree.name + nextUid()) = newTree
        builderStack.prepend(newTree)
    }

    // Pop a parser off the parser callstack.
    def pop(): Unit =
        if (builderStack.isEmpty) {
            // Shouldn't happen, but just in case.
            println("WARNING: Parser stack underflow on pop. This should not have happened!")
        } else {
            // Remove first parser off stack, as if returning from that parser.
            builderStack.remove(0)
            () // XXX: Silences discarded non-unit value warning.
        }
}

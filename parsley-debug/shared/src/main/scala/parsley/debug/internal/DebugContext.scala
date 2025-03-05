/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debug.internal

import scala.collection.mutable

import parsley.XAssert
import parsley.debug.ParseAttempt
import parsley.internal.deepembedding.frontend.LazyParsley
import parsley.debug.DebugView
import parsley.debug.RefCodec.CodedRef

// Class used to hold details about a parser being debugged.
// This is normally held as a value inside an implicit variable.
// Anything caught by the toStringRules will have a parse result of that type toString-ed for memory
// efficiency.

private [parsley] class DebugContext(private val toStringRules: PartialFunction[Any, Boolean], private val view: DebugView) {
    // Create a new dummy root of the tree that will act as filler for the rest of the tree to build
    // off of (as there is no "nil" representation for the tree... other than null, which should be
    // avoided in Scala wherever possible).
    private val dummyRoot = new TransientDebugTree("ROOT", "ROOT", "NIL")

    // Context's checkStack vanished.
    // Migrating it here for futureproofing.
    private val checkStack = mutable.ListBuffer.empty[(Int, Int, Int)]

    def pushPos(offset: Int, line: Int, col: Int): Unit = checkStack.prepend((offset, line, col))
    def popPos(): (Int, Int, Int) = checkStack.remove(0)

    def shouldString(x: Any): Boolean = toStringRules.applyOrElse[Any, Boolean](x, _ => false)

    // Tracks where we are in the parser callstack.
    private val builderStack = mutable.ListBuffer[TransientDebugTree](dummyRoot)

    // Get the final DebugTree from this context.
    def getFinalTree: TransientDebugTree = {
        // The root tree exists only as a placeholder for the rest of the debug tree to build off of.
        // If it has no children, that means the debug tree was not built to begin with.
        // If it was multiple children, somehow the debugger has popped too many tree nodes off the stack.
        val ch = builderStack.head.children
        XAssert.assert(!(ch.size < 1), s"The root tree has somehow lost its only child. (${ch.size})")
        XAssert.assert(!(ch.size > 1), s"The root tree has somehow gained multiple children. (${ch.size})")

        // This should never fail.
        ch.head.withoutBreakpoints().withoutNewlyGeneratedFlags()
    }

    // Add an attempt of parsing at the current stack point.
    def addParseAttempt(attempt: ParseAttempt): Unit = {
        builderStack.head.parse = Some(attempt)

        // This child has consumed part of the parent's input. Add a hole.
        if (attempt.fromOffset != attempt.toOffset && builderStack(1) != dummyRoot) {
            val uuid = builderStack(1).augmentInput(attempt.fromOffset, attempt.toOffset)
            builderStack.head.cNumber = Some(uuid)
        }
    }

    // Reset this context back to zero.
    def reset(): Unit = {
        // Clear anything hanging off the dummy root.
        dummyRoot.children.clear()
        checkStack.clear()

        // The builder stack always starts with just the dummy root, so children can hang off it while building.
        builderStack.clear()
        builderStack.append(dummyRoot)
    }

    // Unique parser IDs.
    /*private var uid = -1L
    private def nextUid(): Long = {
        uid += 1L
        uid
    }*/

    /** The number of breakpoints to skip through.
      *
      * When breakpointSkips is zero, the next breakpoint will stop the parsing.
      */
    private var breakpointSkips: Int = 0

    /** True if associated DebugView extends the Manageable trait */
    def manageableView: Boolean = view match {
        case _: DebugView.Manageable => true 
        case _ => false
    } 

    private var firstBreakpoint: Boolean = true

    /** Trigger a breakpoint.
      *
      * @param fullInput    The full parser input.
      * @param refs         References managed by this breakpoint.
      */
    def triggerBreak(fullInput: String, codedRefs: Option[Seq[CodedRef]]): Option[Seq[CodedRef]] = {
        val debugTree: TransientDebugTree = builderStack.head
        if (firstBreakpoint) {
            debugTree.resetNewlyGeneratedFlags()
            firstBreakpoint = false
        }

        view match {
            case view: DebugView.Pauseable => {
                if (breakpointSkips > 0) { // Skip to next breakpoint
                    breakpointSkips -= 1
                } else if (breakpointSkips != -1) { // Breakpoint exit
                    view match {
                        case view: DebugView.Manageable => {
                            
                            // Wait for RemoteView to return breakpoint skips and updated state
                            val (newSkips, newRefs): (Int, Seq[CodedRef]) = view.renderManage(fullInput, debugTree, codedRefs.get*)
                            
                            debugTree.resetNewlyGeneratedFlags()
                            
                            // Update breakpoint skips
                            breakpointSkips = newSkips
                            return Some(newRefs)
                        }

                        // Update breakpoint using Pausable render call
                        case _ => breakpointSkips = view.renderWait(fullInput, debugTree)
                    }
                    
                }
            }
            
            case _ =>
        }

        debugTree.resetNewlyGeneratedFlags()
        None
    }

    // Push a new parser onto the parser callstack.
    def push(fullInput: String, parser: LazyParsley[_], isIterative: Boolean, userAssignedName: Option[String]): Unit = {

        val newTree = new TransientDebugTree(fullInput = fullInput)
        newTree.name = Renamer.nameOf(userAssignedName, parser)
        newTree.internal = Renamer.internalName(parser)
        newTree.iterative = isIterative

        //val uid = nextUid()
        //builderStack.head.children(s"${newTree.name}-#$uid") = newTree
        builderStack.head.children += newTree
        builderStack.prepend(newTree)
    }

    // Pop a parser off the parser callstack.
    def pop(): Unit = {
        // $COVERAGE-OFF$
        assert(builderStack.nonEmpty, "Parser stack underflow on pop.")
        // $COVERAGE-ON$
        // Remove first parser off stack, as if returning from that parser.
        builderStack.remove(0).applyInputAugments()
    }
}

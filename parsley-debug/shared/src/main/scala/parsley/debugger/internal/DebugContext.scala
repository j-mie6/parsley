/*
 * Copyright (c) 2020, Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors
 *    may be used to endorse or promote products derived from this software without
 *    specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package parsley.debugger.internal

import scala.collection.mutable.ListBuffer

import parsley.debugger.ParseAttempt

import parsley.internal.deepembedding.frontend.LazyParsley

// Class used to hold details about a parser being debugged.
// This is normally held as a value inside an implicit variable.
private [parsley] class DebugContext {
    // Create a new dummy root of the tree that will act as filler for the rest of the tree to build
    // off of (as there is no "nil" representation for the tree... other than null, which should be
    // avoided in Scala wherever possible).
    private def dummyRoot: DebugTreeBuilder =
        DebugTreeBuilder(None, TransientDebugTree("ROOT", "ROOT", "NIL"))

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
    private var builderStack: ListBuffer[DebugTreeBuilder] =
        ListBuffer(dummyRoot)

    // Get the final DebugTreeBuilder from this context.
    def getFinalBuilder: DebugTreeBuilder = {
        // The root tree exists only as a placeholder for the rest of the debug tree to build off of.
        // If it has no children, that means the debug tree was not built to begin with.
        // If it was multiple children, somehow the debugger has popped too many tree nodes off the stack.
        assert(builderStack.head.bChildren.size == 1, "The root tree has somehow lost its only child, or gained multiple children.")

        // This should never fail.
        builderStack.head.bChildren.collectFirst { case (_, x) => x }.get
    }

    // Add an attempt of parsing at the current stack point.
    def addParseAttempt(attempt: ParseAttempt): Unit =
        builderStack.head.node.parse = Some(attempt)

    // Reset this context back to zero.
    def reset(): Unit = {
        builderStack = ListBuffer(dummyRoot)
        checkStack   = ListBuffer()
    }

    // Push a new parser onto the parser callstack.
    def push(fullInput: String, parser: LazyParsley[_], optName: Option[String]): Unit = {
        // This forces a new tree node to be built every time a parser is visited.
        lazy val uniq: Unique[LazyParsley[_]] = Unique(parser)

        if (builderStack.head.bChildren.contains(uniq)) {
            builderStack.prepend(builderStack.head.bChildren(uniq))
        } else {
            val newTree = TransientDebugTree(fullInput = fullInput)
            newTree.name = Rename(optName, parser)
            newTree.internal = Rename.partial(parser)

            val dtb = DebugTreeBuilder(optName, newTree)

            builderStack.head.bChildren(uniq) = dtb
            builderStack.prepend(dtb)
        }
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

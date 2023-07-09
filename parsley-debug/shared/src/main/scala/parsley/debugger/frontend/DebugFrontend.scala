/* SPDX-FileCopyrightText: © 2023 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debugger.frontend

import parsley.debugger.DebugTree

/** Trait representing a debug frontend for a debugger to present the debug tree.
  *
  * Any compliant implementation that handles all nodes of a [[parsley.debugger.DebugTree]] can be
  * used in place of any other implementation (e.g. a serialiser to JSON, a GUI, etc.).
  *
  * All implementations of this trait that are intended to be available publicly (e.g. within an
  * addon library for the debugger) must live in the package `parsley.debugger.frontend`, next
  * to this trait.
  */
trait DebugFrontend {
    /** Process a debug tree using whatever the frontend is doing to present the tree in some way.
      *
      * @param tree Debug tree to process.
      */
    def process(input: => String, tree: => DebugTree): Unit
}

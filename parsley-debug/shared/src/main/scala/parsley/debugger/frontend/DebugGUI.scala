/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debugger.frontend

import parsley.debugger.DebugTree

/** Trait representing a GUI frontend for a debugger to display things.
  *
  * Although named as such, it is subject to change as "GUI" is defined very loosely, and any
  * compliant implementation that handles all nodes of a [[parsley.debugger.DebugTree]] can be used
  * in place of any other implementation (e.g. a serialiser to JSON).
  */
trait DebugGUI {
  /** Render a debug tree using whatever the implementer is rendering with.
    *
    * @param tree Debug tree to render.
    */
  def render(input: => String, tree: => DebugTree): Unit
}

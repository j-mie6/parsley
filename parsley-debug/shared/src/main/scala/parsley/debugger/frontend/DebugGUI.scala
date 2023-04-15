/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debugger.frontend

import parsley.debugger.DebugTree

/** Trait representing a GUI frontend for a debugger to display things. */
trait DebugGUI {
  /** Render a debug tree using whatever the implementer is rendering with.
    *
    * @param tree Debug tree to render.
    */
  def render(tree: => DebugTree): Unit
}

package parsley.debugger.objects

/** Trait representing a GUI frontend for a debugger to display things. */
trait DebugGUI {
  /** Render a debug tree using whatever the implementer is rendering with.
    *
    * @param tree Debug tree to render.
    */
  def render(tree: DebugTree): Unit
}

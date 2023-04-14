package parsley.debugger.frontend

import parsley.debugger.objects.DebugTree

/** Internal implementation for console printer child class. */
class ConsoleGUI private () extends DebugGUI {
  override def render(tree: DebugTree): Unit =
    println(tree)
}

/** A console pretty-printer for the debugger.
  *
  * Will automatically print the debug tree into the console after a parser has finished running.
  *
  * Technically not a GUI.
  */
object ConsoleGUI {
  /** Create an instance of the console pretty-printer.
    *
    * It is recommended that you assign this to an implicit value before calling
    * [[parsley.debugger.combinators.attachDebuggerGUI]].
    *
    * @return Console instance for pretty-printing debug trees.
    */
  def newInstance: ConsoleGUI = new ConsoleGUI()
}

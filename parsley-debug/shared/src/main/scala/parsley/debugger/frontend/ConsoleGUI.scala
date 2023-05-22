package parsley.debugger.frontend

import parsley.debugger.DebugTree

/** A console pretty-printer for the debugger.
  *
  * Will automatically print the debug tree into the console after a parser has finished running.
  *
  * Technically not a GUI.
  */
case object ConsoleGUI extends DebugGUI {
  override def render(input: => String, tree: => DebugTree): Unit = {
    println(s"${tree.parserName}'s parse tree for input:\n\n${input}\n")
    println(tree)
  }
}

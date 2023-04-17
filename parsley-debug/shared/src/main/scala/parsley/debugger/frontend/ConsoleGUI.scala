package parsley.debugger.frontend

import parsley.debugger.DebugTree

/** A console pretty-printer for the debugger.
  *
  * Will automatically print the debug tree into the console after a parser has finished running.
  *
  * Technically not a GUI.
  */
case object ConsoleGUI extends DebugGUI {
  /** Get an instance of the console pretty-printer.
    *
    * It is recommended that you assign this to an implicit value before calling
    * [[parsley.debugger.attachDebuggerGUI]].
    *
    * @return Console instance for pretty-printing debug trees.
    */
  def newInstance: ConsoleGUI.type = {
    // Other GUIs may not be singletons like this class as they may contain
    // state information required to run them.
    // This particular class can be a singleton as it is stateless.
    this
  }

  override def render(input: => String, tree: => DebugTree): Unit = {
    println(s"${tree.parserName}'s parse tree for input:\n\n${input}\n")
    println(tree)
  }
}

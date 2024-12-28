/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debugger.frontend

import parsley.debugger.DebugTree
import parsley.debugger.frontend.internal.consolepretty.*

private [frontend] sealed class ConsolePrettyPrinterImpl private[frontend] (ioF: String => Unit) extends ReusableFrontend {
    override private [debugger] def process(input: => String, tree: => DebugTree): Unit = {
        ioF(s"${tree.parserName}'s parse tree for input:\n\n$input\n\n")
        ioF(tree.pretty + "\n")
    }
}

/** A console pretty-printer for the debugger.
  *
  * It is recommended that all memory-heavy types (e.g. closures) are not stored explicitly. Consult the documentation
  * on attaching debuggers to find out how to prevent that.
  *
  * @since 5.0.0
  */
object ConsolePrettyPrinter extends ConsolePrettyPrinterImpl(println(_)) {
    /** Create a string pretty-printer that takes an arbitrary impure string function.
      *
      * @since 5.0.0
      */
    def apply(ioF: String => Unit): ReusableFrontend = new ConsolePrettyPrinterImpl(ioF)
}

/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
// $COVERAGE-OFF$
package parsley.internal.machine.instructions

import parsley.XAssert._
import parsley.errors.ErrorBuilder

import parsley.internal.errors.{ExpectItem, FancyError, ParseError, TrivialError}
import parsley.internal.machine.{Context, Failed, Finished, Good, Recover}
import parsley.internal.machine.XAssert._

import Indenter.indentAndUnlines
import InputSlicer.Pad
import PrettyPortal.{Direction, Enter, Exit}

private [instructions] trait Colours {
    val ascii: Boolean

    final val newline = green("↙")
    final val space = white("·")
    final val endOfInput = red("•")

    final private def colour(str: String, colour: String): String = {
        if (ascii || parsley.debug.renderAscii) str else s"$colour$str${Console.RESET}"
    }
    final protected def green(str: String): String = colour(str, Console.GREEN)
    final protected def blue(str: String): String = colour(str, Console.BLUE)
    final protected def red(str: String): String = colour(str, Console.RED)
    final protected def white(str: String): String = colour(str, Console.WHITE)
}

private [instructions] trait PrettyPortal {
    val name: String

    final protected def portal(dir: Direction, ctx: Context) = s"${dir.render}$name${dir.render}"
}
private [instructions] object PrettyPortal {
    sealed abstract class Direction {
        def render: String
    }
    case object Enter extends Direction {
        override val render: String = ">"
    }
    case object Exit extends Direction {
        override val render: String = "<"
    }
}

private [instructions] object Indenter {
    final val IndentWidth: Int = 2
    private def indent(ctx: Context) = " " * (ctx.debuglvl * 2)
    def indentAndUnlines(ctx: Context, lines: String*): String = {
        lines.map(line => s"${indent(ctx)}$line").mkString("\n")
    }
}

private [instructions] trait InputSlicer { this: Colours =>
    private def start(ctx: Context): Int = Math.max(ctx.offset - Pad, 0)
    private def end(ctx: Context): Int = Math.min(ctx.offset + Pad + 1, ctx.inputsz)
    protected final def slice(ctx: Context): String = {
        val end = this.end(ctx)
        val s = ctx.input.mkString.substring(start(ctx), end).replace("\n", newline)
                                                             .replace(" ", space)
        if (end == ctx.inputsz) s"$s$endOfInput" else s
    }
    protected final def caret(ctx: Context): String = {
        " " * (ctx.offset - start(ctx)) + blue("^")
    }
}
private [instructions] object InputSlicer {
    final val Pad: Int = 5
}

private [instructions] trait Logger extends PrettyPortal with InputSlicer with Colours {
    final protected def preludeString(dir: Direction, ctx: Context, ends: String) = {
        val input = this.slice(ctx)
        val prelude = s"${portal(dir, ctx)} (${ctx.line}, ${ctx.col}): "
        val caret = (" " * prelude.length) + this.caret(ctx)
        indentAndUnlines(ctx, s"$prelude$input$ends", caret)
    }
    final protected def doBreak(ctx: Context): Unit = {
        print(indentAndUnlines(ctx,
                s"{stack: ${ctx.stack.mkString(", ")}}",
                s"{registers: ${ctx.regs.zipWithIndex.map{case (x, i) => s"r$i: $x"}.mkString("[", ", ", "])}")}}",
                "..."))
        Console.in.read()
    }
}

private [internal] final class LogBegin(var label: Int, override val name: String, override val ascii: Boolean, break: Boolean)
    extends InstrWithLabel with Logger {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        println(preludeString(Enter, ctx, ""))
        if (break) doBreak(ctx)
        ctx.debuglvl += 1
        ctx.pushHandler(label)
        ctx.inc()
    }
    override def toString: String = s"LogBegin($label, $name)"
}

private [internal] final class LogEnd(val name: String, override val ascii: Boolean, break: Boolean) extends Instr with Logger {
    override def apply(ctx: Context): Unit = {
        assert(ctx.running, "cannot wrap a Halt with a debug")
        ctx.debuglvl -= 1
        val end = " " + {
            if (ctx.good) {
                ctx.handlers = ctx.handlers.tail
                ctx.inc()
                green("Good")
            }
            else {
                ctx.fail()
                red("Fail")
            }
        }
        println(preludeString(Exit, ctx, end))
        if (break) doBreak(ctx)
    }
    override def toString: String = s"LogEnd($name)"
}

private [instructions] trait ErrLogger extends PrettyPortal with Colours {
    final protected def preludeString(dir: Direction, ctx: Context, ends: String) = {
        val prelude = s"${portal(dir, ctx)} (offset ${ctx.offset}, line ${ctx.line}, col ${ctx.col})"
        indentAndUnlines(ctx, s"$prelude$ends")
    }
    /*
    final protected def doBreak(ctx: Context): Unit = {
        print(indentAndUnlines(ctx,
                s"{stack: ${ctx.stack.mkString(", ")}}",
                s"{registers: ${ctx.regs.zipWithIndex
                .map{case (x, i) => s"r$i: $x"}.mkString("[", ", ", "])}")}}",
                "..."))
        Console.in.read()
    }
    */
}

private [instructions] final case class ErrLogData(hintsOffset: Int, hints: Set[ExpectItem]) {
    def newHints(newHintsOffset: Int, newHints: Set[ExpectItem]): Set[ExpectItem] = {
        if (stillValid(newHintsOffset)) newHints &~ hints
        else newHints
    }
    def stillValid(newHintsOffset: Int): Boolean = hintsOffset == newHintsOffset
}

private [internal] final class LogErrBegin(var label: Int, override val name: String, override val ascii: Boolean)(implicit errBuilder: ErrorBuilder[_])
    extends InstrWithLabel with ErrLogger {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        val inFlightHints = ctx.inFlightHints.toSet
        // This should print out a classic opening line, followed by the currently in-flight hints
        println(preludeString(Enter, ctx, s": current hints are ${inFlightHints.map(_.formatExpect)} (valid at offset ${ctx.currentHintsValidOffset})"))
        ctx.debuglvl += 1
        ctx.stack.push(ErrLogData(ctx.currentHintsValidOffset, inFlightHints))
        ctx.pushHandler(label)
        ctx.inc()
    }
    override def toString: String = s"LogErrBegin($label, $name)"
}

private [internal] final class LogErrEnd(override val name: String, override val ascii: Boolean)(implicit errBuilder: ErrorBuilder[_])
    extends Instr with ErrLogger {
    override def apply(ctx: Context): Unit = {
        assert(ctx.running, "cannot wrap a Halt with a debug")
        ctx.debuglvl -= 1
        val currentHintsValidOffset = ctx.currentHintsValidOffset
        if (ctx.good) {
            // In this case, the currently in-flight hints should be reported
            val oldData = ctx.stack.pop[ErrLogData]()
            val inFlightHints = ctx.inFlightHints.toSet
            val formattedInFlight = inFlightHints.map(_.formatExpect)
            val msgInit = s": ${green("Good")}, current hints are $formattedInFlight with"
            ctx.handlers = ctx.handlers.tail
            if (!oldData.stillValid(ctx.currentHintsValidOffset)) {
                println(preludeString(Exit, ctx, s"$msgInit old hints discarded (valid at offset ${ctx.currentHintsValidOffset})"))
            }
            else {
                val newHints = oldData.newHints(ctx.currentHintsValidOffset, inFlightHints)
                if (newHints.size == inFlightHints.size) {
                    println(preludeString(Exit, ctx, s"$msgInit all added since entry to debug (valid at offset ${ctx.currentHintsValidOffset})"))
                }
                else {
                    val formattedNewHints = oldData.newHints(ctx.currentHintsValidOffset, inFlightHints).map(_.formatExpect)
                    val msg = s"$msgInit $formattedNewHints added since entry to debug (valid at offset ${ctx.currentHintsValidOffset})"
                    println(preludeString(Exit, ctx, msg))
                }
            }
            ctx.inc()
        }
        else {
            // In this case, the current top of stack error message is reported
            // For this, there needs to be a verbose mode that prints out the composite error
            // as opposed to the simplified error (post-merge)
            // there should be different levels of verbose flags, such that we can also display the expecteds _under_ a label
            val oldData = ctx.stack.peek[ErrLogData]
            val defuncErr = ctx.inFlightError
            val err = defuncErr.asParseError(ctx.errorItemBuilder)
            println(preludeString(Exit, ctx, s": ${red("Fail")}"))
            println(Indenter.indentAndUnlines(ctx, LogErrEnd.format(err): _*))
            ctx.fail()
        }
    }
    override def toString: String = s"LogErrEnd($name)"
}
private [instructions] object LogErrEnd {
    // TODO: We want to mark errors that are behind the current context offsets as amended in some way
    // ideally we can provide meta-information about merges and so on too: this needs to be extracted by
    // analysing the DefuncError itself! (Note that StringTok does not amend its error via operation,
    // it does so by direct manipulation. It should be treated as an amend regardless here).
    // Surpressed labels can be presumably captured by calling asParseErrorSlow under a label node
    // again, requiring a deeper traversal of the tree
    def format(err: ParseError): Seq[String] = err match {
        case FancyError(offset, line, col, msgs, _) => s"generated specialised error (offset $offset, line $line, col $col) {" +: msgs :+ "}"
        case TrivialError(offset, line, col, unexpected, expecteds, reasons, lexicalError) =>
            Seq(s"generated vanilla error (offset $offset, line $line, col $col) {",
                s"  unexpected item = ${unexpected.fold("missing")(_.formatUnexpect(lexicalError)._1.toString)}",
                s"  expected item(s) = ${expecteds.map(_.formatExpect)}",
                s"  reasons =${if (reasons.isEmpty) " no reasons given" else ""}") ++
                reasons.map("  " + _) :+
                "}"
    }
}
// $COVERAGE-ON$

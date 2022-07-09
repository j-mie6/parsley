/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
// $COVERAGE-OFF$
package parsley.internal.machine.instructions

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

private [internal] final class LogBegin(var label: Int, val name: String, val ascii: Boolean, break: Boolean) extends InstrWithLabel with Logger {
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

private [internal] final class LogEnd(val name: String, val ascii: Boolean, break: Boolean) extends Instr with Logger {
    override def apply(ctx: Context): Unit = {
        ctx.debuglvl -= 1
        val end = " " + (ctx.status match {
            case Good             =>
                ctx.handlers = ctx.handlers.tail
                ctx.inc()
                green("Good")
            case Recover | Failed =>
                ctx.fail()
                red("Fail")
            case Finished         => throw new Exception("debug cannot wrap a halt?!") // scalastyle:ignore throw
        })
        println(preludeString(Exit, ctx, end))
        if (break) doBreak(ctx)
    }
    override def toString: String = s"LogEnd($name)"
}

private [instructions] trait ErrLogger extends PrettyPortal with Colours {
    final protected def preludeString(dir: Direction, ctx: Context, ends: String) = {
        val prelude = s"${portal(dir, ctx)} (${ctx.line}, ${ctx.col})"
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

private [internal] final class LogErrBegin(var label: Int, val name: String, val ascii: Boolean) extends InstrWithLabel with ErrLogger {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        println(preludeString(Enter, ctx, ""))
        ctx.debuglvl += 1
        // This should print out a classic opening line, followed by the currently in-flight hints
        println(ctx.inFlightHints)
        ctx.stack.push(ctx.currentHintsValidOffset)
        ctx.pushHandler(label)
        ctx.inc()
    }
    override def toString: String = s"LogErrBegin($label, $name)"
}

private [internal] final class LogErrEnd(val name: String, val ascii: Boolean) extends Instr with ErrLogger {
    override def apply(ctx: Context): Unit = {
        ctx.debuglvl -= 1
        val currentHintsValidOffset = ctx.currentHintsValidOffset
        ctx.status match {
            case Good             =>
                // In this case, the currently in-flight hints should be reported
                val entryHintsValidOffset = ctx.stack.pop[Int]()
                ctx.handlers = ctx.handlers.tail
                println(preludeString(Exit, ctx, green(": Good")))
                println(ctx.inFlightHints.toSet)
                println(s"$entryHintsValidOffset -> $currentHintsValidOffset")
                ctx.inc()
            case Recover | Failed =>
                // In this case, the current top of stack error message is reported
                // For this, there needs to be a verbose mode that prints out the composite error
                // as opposed to the simplified error (post-merge)
                // there should be different levels of verbose flags, such that we can also display the expecteds _under_ a label
                val entryHintsValidOffset = ctx.stack.peek[Int]
                println(preludeString(Exit, ctx, red(": Fail")))
                println(ctx.inFlightError)
                println(s"$entryHintsValidOffset -> $currentHintsValidOffset")
                ctx.fail()
            case Finished         => throw new Exception("debug cannot wrap a halt?!") // scalastyle:ignore throw
        }
    }
    override def toString: String = s"LogErrEnd($name)"
}
// $COVERAGE-ON$

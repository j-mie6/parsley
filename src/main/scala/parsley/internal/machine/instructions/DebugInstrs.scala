package parsley.internal.machine.instructions

import parsley.internal.errors.{Desc, ErrorItem}
import parsley.internal.machine.{Context, Failed, Finished, Good, Recover}
import parsley.internal.machine.errors.EmptyError

// $COVERAGE-OFF$
private [instructions] trait Logger {
    val name: String
    val ascii: Boolean
    final val newline = green("↙")
    final val space = white("·")
    final val endOfInput = red("•")
    final protected def preludeString(dir: Char, ctx: Context, ends: String = "") = {
        val indent = this.indent(ctx)
        val start = Math.max(ctx.offset - 5, 0)
        val end = Math.min(ctx.offset + 6, ctx.inputsz)
        val input = ctx.input.mkString.substring(start, end).replace("\n", newline)
                                                            .replace(" ", space)
        val inputAndEof = if (end == ctx.inputsz) input + endOfInput else input
        val prelude = s"$indent$dir$name$dir (${ctx.line}, ${ctx.col}): "
        val caret = " " * (prelude.length + ctx.offset - start) + blue("^")
        s"$prelude$inputAndEof$ends\n$caret"
    }
    final protected def doBreak(ctx: Context): Unit = {
        print(s"${indent(ctx)}{stack: ${ctx.stack.mkString(", ")}})\n" +
              s"${indent(ctx)}{registers: ${ctx.regs.zipWithIndex.map{case (x, i) => s"r$i: $x"}.mkString("[", ", ", "])}")}}\n" +
              s"${indent(ctx)}...")
        Console.in.read()
    }
    final protected def indent(ctx: Context) = " " * (ctx.debuglvl * 2)
    final private def colour(str: String, colour: String): String = if (ascii || parsley.debug.renderAscii) str else s"$colour$str${Console.RESET}"
    final protected def green(str: String): String = colour(str, Console.GREEN)
    final protected def blue(str: String): String = colour(str, Console.BLUE)
    final protected def red(str: String): String = colour(str, Console.RED)
    final protected def white(str: String): String = colour(str, Console.WHITE)
}

private [internal] final class LogBegin(var label: Int, val name: String, val ascii: Boolean, break: Boolean) extends InstrWithLabel with Logger {
    override def apply(ctx: Context): Unit = {
        println(preludeString('>', ctx))
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
        println(preludeString('<', ctx, end))
        if (break) doBreak(ctx)
    }
    override def toString: String = s"LogEnd($name)"
}

private [internal] final class LogErrBegin(var label: Int, val name: String) extends InstrWithLabel {
    override def apply(ctx: Context): Unit = {
        ctx.debuglvl += 1
        // This should print out a classic opening line, followed by the currently in-flight hints
        ctx.pushHandler(label)
        ctx.inc()
    }
    override def toString: String = s"LogErrBegin($label, $name)"
}

private [internal] final class LogErrEnd(val name: String) extends Instr {
    override def apply(ctx: Context): Unit = {
        ctx.debuglvl -= 1
        ctx.status match {
            case Good             =>
                // In this case, the currently in-flight hints should be reported
                ctx.handlers = ctx.handlers.tail
                ctx.inc()
            case Recover | Failed =>
                // In this case, the current top of stack error message is reported
                // For this, there needs to be a verbose mode that prints out the composite error
                // as opposed to the simplified error (post-merge)
                // there should be different levels of verbose flags, such that we can also display the expecteds _under_ a label
                ctx.fail()
            case Finished         => throw new Exception("debug cannot wrap a halt?!") // scalastyle:ignore throw
        }
    }
    override def toString: String = s"LogErrEnd($name)"
}
// $COVERAGE-ON$

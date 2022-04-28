/* SPDX-FileCopyrightText: © 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.instructions

import scala.annotation.tailrec

import parsley.internal.errors.{Desc, ErrorItem}
import parsley.internal.machine.{Context, Failed, Finished, Good, Recover}
import parsley.internal.machine.errors.EmptyError

// Stack Manipulators
private [internal] final class Push[A](x: A) extends Instr {
    override def apply(ctx: Context): Unit = ctx.pushAndContinue(x)
    // $COVERAGE-OFF$
    override def toString: String = s"Push($x)"
    // $COVERAGE-ON$
}

private [internal] object Pop extends Instr {
    override def apply(ctx: Context): Unit = {
        ctx.stack.pop_()
        ctx.inc()
    }
    // $COVERAGE-OFF$
    override def toString: String = "Pop"
    // $COVERAGE-ON$
}

/*private [internal] object Swap extends Instr {
    override def apply(ctx: Context): Unit = {
        val y = ctx.stack.upop()
        val x = ctx.stack.peekAndExchange(y)
        ctx.unsafePushAndContinue(x)
    }
    // $COVERAGE-OFF$
    override def toString: String = "Swap"
    // $COVERAGE-ON$
}*/

// Applicative Functors
private [internal] object Apply extends Instr {
    override def apply(ctx: Context): Unit = {
        val x = ctx.stack.upop()
        val f = ctx.stack.peek[Any => Any]
        ctx.exchangeAndContinue(f(x))
    }
    // $COVERAGE-OFF$
    override def toString: String = "Apply"
    // $COVERAGE-ON$
}

// Monadic
private [internal] final class DynCall[-A](f: A => Array[Instr]) extends Instr {
    private [DynCall] val g = f.asInstanceOf[Any => Array[Instr]]
    override def apply(ctx: Context): Unit = ctx.call(g(ctx.stack.upop()))
    // $COVERAGE-OFF$
    override def toString: String = "DynCall(?)"
    // $COVERAGE-ON$
}

// Control Flow
private [internal] object Halt extends Instr {
    override def apply(ctx: Context): Unit = ctx.status = Finished
    // $COVERAGE-OFF$
    override def toString: String = "Halt"
    // $COVERAGE-ON$
}

private [internal] final class Call(var label: Int) extends InstrWithLabel {
    private var isSet: Boolean = false
    override def relabel(labels: Array[Int]): this.type = {
        if (!isSet) {
            label = labels(label)
            isSet = true
        }
        this
    }
    private [internal] var preserve: Array[Int] = _

    override def apply(ctx: Context): Unit = ctx.call(label, preserve)
    // $COVERAGE-OFF$
    override def toString: String = s"Call($label)"
    // $COVERAGE-ON$
}

private [internal] final class GoSub(var label: Int) extends InstrWithLabel {
    override def apply(ctx: Context): Unit = ctx.call(label)
    // $COVERAGE-OFF$
    override def toString: String = s"GoSub($label)"
    // $COVERAGE-ON$
}

private [internal] object Return extends Instr {
    override def apply(ctx: Context): Unit = ctx.ret()
    // $COVERAGE-OFF$
    override def toString: String = "Return"
    // $COVERAGE-ON$
}

private [internal] object Empty extends Instr {
    override def apply(ctx: Context): Unit = ctx.fail(new EmptyError(ctx.offset, ctx.line, ctx.col))
    // $COVERAGE-OFF$
    override def toString: String = "Empty"
    // $COVERAGE-ON$
}

private [internal] final class PushHandler(var label: Int) extends InstrWithLabel {
    override def apply(ctx: Context): Unit = {
        ctx.pushHandler(label)
        ctx.inc()
    }
    // $COVERAGE-OFF$
    override def toString: String = s"PushHandler($label)"
    // $COVERAGE-ON$
}

private [internal] object PopHandler extends Instr {
    override def apply(ctx: Context): Unit = {
        ctx.handlers = ctx.handlers.tail
        ctx.inc()
    }
    // $COVERAGE-OFF$
    override def toString: String = "PopHandler"
    // $COVERAGE-ON$
}

private [internal] final class PushHandlerAndState(var label: Int, saveHints: Boolean, hideHints: Boolean) extends InstrWithLabel {
    override def apply(ctx: Context): Unit = {
        ctx.pushHandler(label)
        ctx.saveState()
        if (saveHints) ctx.saveHints(shadow = hideHints)
        ctx.inc()
    }
    // $COVERAGE-OFF$
    override def toString: String = s"PushHandlerAndState($label)"
    // $COVERAGE-ON$
}

private [internal] object PopHandlerAndState extends Instr {
    override def apply(ctx: Context): Unit = {
        ctx.states = ctx.states.tail
        ctx.handlers = ctx.handlers.tail
        ctx.inc()
    }
    // $COVERAGE-OFF$
    override def toString: String = "PopHandlerAndState"
    // $COVERAGE-ON$
}

private [internal] final class PushHandlerAndCheck(var label: Int, saveHints: Boolean) extends InstrWithLabel {
    override def apply(ctx: Context): Unit = {
        ctx.pushCheck()
        ctx.pushHandler(label)
        if (saveHints) ctx.saveHints(false)
        ctx.inc()
    }
    // $COVERAGE-OFF$
    override def toString: String = s"PushHandlerAndCheck($label)"
    // $COVERAGE-ON$
}

private [internal] object PopHandlerAndCheck extends Instr {
    override def apply(ctx: Context): Unit = {
        ctx.checkStack = ctx.checkStack.tail
        ctx.handlers = ctx.handlers.tail
        ctx.inc()
    }
    // $COVERAGE-OFF$
    override def toString: String = "PopHandlerAndCheck"
    // $COVERAGE-ON$
}

private [internal] final class Jump(var label: Int) extends InstrWithLabel {
    override def apply(ctx: Context): Unit = ctx.pc = label
    // $COVERAGE-OFF$
    override def toString: String = s"Jump($label)"
    // $COVERAGE-ON$
}

private [internal] final class JumpAndPopCheck(var label: Int) extends InstrWithLabel {
    override def apply(ctx: Context): Unit = {
        ctx.handlers = ctx.handlers.tail
        ctx.checkStack = ctx.checkStack.tail
        ctx.commitHints()
        ctx.pc = label
    }
    // $COVERAGE-OFF$
    override def toString: String = s"JumpAndPopCheck($label)"
    // $COVERAGE-ON$
}

private [internal] final class JumpAndPopState(var label: Int) extends InstrWithLabel {
    override def apply(ctx: Context): Unit = {
        ctx.handlers = ctx.handlers.tail
        ctx.states = ctx.states.tail
        ctx.commitHints()
        ctx.pc = label
    }
    // $COVERAGE-OFF$
    override def toString: String = s"JumpAndPopState($label)"
    // $COVERAGE-ON$
}

private [internal] final class Catch(var label: Int) extends InstrWithLabel {
    override def apply(ctx: Context): Unit = {
        ctx.restoreHints()
        ctx.catchNoConsumed {
            ctx.pushHandler(label)
            ctx.inc()
        }
    }
    // $COVERAGE-OFF$
    override def toString: String = s"Catch($label)"
    // $COVERAGE-ON$
}

private [internal] final class RestoreAndPushHandler(var label: Int) extends InstrWithLabel {
    override def apply(ctx: Context): Unit = {
        ctx.restoreState()
        ctx.restoreHints()
        ctx.status = Good
        ctx.pushHandler(label)
        ctx.inc()
    }
    // $COVERAGE-OFF$
    override def toString: String = s"RestoreAndPushHandler($label)"
    // $COVERAGE-ON$
}

// Debugging Instructions
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
// $COVERAGE-ON$

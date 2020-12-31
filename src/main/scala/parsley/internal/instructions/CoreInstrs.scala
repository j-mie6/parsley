package parsley.internal.instructions

import Stack.{isEmpty, push}
import parsley.internal.ResizableArray
import parsley.internal.UnsafeOption
import parsley.internal.deepembedding.Parsley

import scala.annotation.tailrec

// Stack Manipulators
private [internal] final class Push[A](x: A) extends Instr {
    override def apply(ctx: Context): Unit = ctx.pushAndContinue(x)
    override def toString: String = s"Push($x)"
}

private [internal] object Pop extends Instr {
    override def apply(ctx: Context): Unit = {
        ctx.stack.pop_()
        ctx.inc()
    }
    override def toString: String = "Pop"
}

private [internal] object Flip extends Instr {
    override def apply(ctx: Context): Unit = {
        val x = ctx.stack(1)
        ctx.stack(1) = ctx.stack.upeek
        ctx.exchangeAndContinue(x)
    }
    override def toString: String = "Flip"
}

// Applicative Functors
private [internal] object Apply extends Instr {
    override def apply(ctx: Context): Unit = {
        val x = ctx.stack.upop()
        val f = ctx.stack.peek[Any => Any]
        ctx.exchangeAndContinue(f(x))
    }
    override def toString: String = "Apply"
}

// Monadic
private [internal] final class DynCall[-A](f: A => Array[Instr], expected: UnsafeOption[String]) extends Instr {
    private [DynCall] val g = f.asInstanceOf[Any => Array[Instr]]
    override def apply(ctx: Context): Unit = ctx.call(g(ctx.stack.upop()), 0, expected)
    override def toString: String = "DynCall(?)"
}

// Control Flow
private [internal] final class Call(_instrs: =>Array[Instr], expected: UnsafeOption[String]) extends Instr {
    private [Call] lazy val (instrs, pindices) = {
        val is = _instrs
        (is, statefulIndices(is))
    }

    override def apply(ctx: Context): Unit = ctx.call(stateSafeCopy(instrs, pindices), 0, expected)
    override def toString: String = "Call"
}

private [internal] final class GoSub(var label: Int, expected: UnsafeOption[String]) extends JumpInstr {
    override def apply(ctx: Context): Unit = ctx.call(ctx.instrs, label, expected)
    override def toString: String = s"GoSub($label)"
}

private [internal] object Return extends Instr {
    override def apply(ctx: Context): Unit = ctx.ret()
    override def toString: String = "Return"
}

private [internal] final class Empty(expected: UnsafeOption[String]) extends Instr {
    override def apply(ctx: Context): Unit = {
        val strip = ctx.expected.isEmpty
        ctx.fail(expected)
        if (strip) ctx.unexpected = null
    }
    override def toString: String = "Empty"
}

private [internal] final class PushHandler(var label: Int) extends JumpInstr {
    override def apply(ctx: Context): Unit = {
        ctx.pushHandler(label)
        ctx.saveState()
        ctx.inc()
    }
    override def toString: String = s"PushHandler($label)"
}

private [internal] final class PushFallthrough(var label: Int) extends JumpInstr {
    override def apply(ctx: Context): Unit = {
        ctx.pushHandler(label)
        ctx.inc()
    }
    override def toString: String = s"PushFallthrough($label)"
}

private [internal] final class InputCheck(var label: Int) extends JumpInstr {
    override def apply(ctx: Context): Unit = {
        ctx.pushCheck()
        ctx.pushHandler(label)
        ctx.inc()
    }
    override def toString: String = s"InputCheck($label)"
}

private [internal] final class Jump(var label: Int) extends JumpInstr {
    override def apply(ctx: Context): Unit = ctx.pc = label
    override def toString: String = s"Jump($label)"
}

private [internal] final class JumpGood(var label: Int) extends JumpInstr {
    override def apply(ctx: Context): Unit = {
        ctx.handlers = ctx.handlers.tail
        ctx.checkStack = ctx.checkStack.tail
        ctx.pc = label
    }
    override def toString: String = s"JumpGood($label)"
}

private [internal] object Catch extends Instr {
    override def apply(ctx: Context): Unit = ctx.catchNoConsumed {
        ctx.inc()
    }
    override def toString: String = s"Catch"
}

// Debugging Instructions
private [instructions] trait Logger {
    val name: String
    final protected def preludeString(dir: Char, ctx: Context, ends: String = "") = {
        val indent = this.indent(ctx)
        val start = Math.max(ctx.offset - 5, 0)
        val end = Math.min(ctx.offset + 6, ctx.inputsz)
        val input = ctx.input.mkString.substring(start, end).replace("\n", Console.GREEN + "↙" + Console.RESET)
                                                            .replace(" ", Console.WHITE + "·" + Console.RESET)
        val inputAndEof = if (end == ctx.inputsz) input + Console.RED + "•" + Console.RESET else input
        val prelude = s"$indent$dir$name$dir (${ctx.line}, ${ctx.col}): "
        val caret = " " * (prelude.length + ctx.offset - start) + Console.BLUE + "^" + Console.RESET
        s"$prelude$inputAndEof$ends\n$caret"
    }
    final protected def doBreak(ctx: Context): Unit = {
        print(s"${indent(ctx)}{stack: ${ctx.stack.mkString(", ")}})\n" +
              s"${indent(ctx)}{registers: ${ctx.regs.zipWithIndex.map{case (x, i) => s"r$i: $x"}.mkString("[", ", ", "])}")}}\n" +
              s"${indent(ctx)}...")
        Console.in.read()
    }
    final protected def indent(ctx: Context) = " " * (ctx.debuglvl * 2)
}

private [internal] final class LogBegin(var label: Int, val name: String, break: Boolean) extends JumpInstr with Logger {
    override def apply(ctx: Context): Unit = {
        println(preludeString('>', ctx))
        if (break) doBreak(ctx)
        ctx.debuglvl += 1
        ctx.pushHandler(label)
        ctx.inc()
    }
    override def toString: String = s"LogBegin($label, $name)"
}

private [internal] final class LogEnd(val name: String, break: Boolean) extends Instr with Logger {
    override def apply(ctx: Context): Unit = {
        ctx.debuglvl -= 1
        val end = " " + (ctx.status match {
            case Good =>
                ctx.handlers = ctx.handlers.tail
                ctx.inc()
                Console.GREEN + "Good" + Console.RESET
            case Recover | Failed =>
                ctx.fail()
                Console.RED + "Fail" + Console.RESET
        })
        println(preludeString('<', ctx, end))
        if (break) doBreak(ctx)
    }
    override def toString: String = s"LogEnd($name)"
}
package parsley.internal.instructions

import Stack.{isEmpty, push}
import parsley.internal.ResizableArray
import parsley.internal.UnsafeOption
import parsley.internal.deepembedding.Parsley

import scala.annotation.tailrec
import scala.language.existentials

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

// Primitives
private [internal] class CharTok(c: Char, x: Any, _expected: UnsafeOption[String]) extends Instr {
    val expected: String = if (_expected == null) "\"" + c + "\"" else _expected
    override def apply(ctx: Context): Unit = {
        if (ctx.moreInput && ctx.nextChar == c) {
            ctx.consumeChar()
            ctx.pushAndContinue(x)
        }
        else ctx.fail(expected)
    }
    override def toString: String = if (x == c) s"Chr($c)" else s"ChrPerform($c, $x)"
}

private [internal] final class Satisfies(f: Char => Boolean, expected: UnsafeOption[String]) extends Instr {
    override def apply(ctx: Context): Unit = {
        if (ctx.moreInput && f(ctx.nextChar)) ctx.pushAndContinue(ctx.consumeChar())
        else ctx.fail(expected)
    }
    override def toString: String = "Sat(?)"
}

private [internal] final class StringTok private [instructions] (s: String, x: Any, _expected: UnsafeOption[String]) extends Instr {
    private [this] val expected = if (_expected == null) "\"" + s + "\"" else _expected
    private [this] val cs = s.toCharArray
    private [this] val sz = cs.length
    private [this] val adjustAtIndex = new Array[(Int => Int, Int => Int)](s.length + 1)
    def makeAdjusters(col: Int, line: Int, tabprefix: Option[Int]): (Int => Int, Int => Int) =
        if (line > 0) ((_: Int) => col, (x: Int) => x + line)
        else (tabprefix match {
            case Some(prefix) =>
                val outer = 4 + col + prefix
                val inner = prefix - 1
                (x: Int) => outer + x - ((x + inner) & 3)
            case None => (x: Int) => x + col
        }, (x: Int) => x)
    @tailrec def compute(cs: Array[Char], i: Int = 0, col: Int = 0, line: Int = 0)(implicit tabprefix: Option[Int] = None): Unit = {
        adjustAtIndex(i) = makeAdjusters(col, line, tabprefix)
        if (i < cs.length) cs(i) match {
            case '\n' => compute(cs, i + 1, 1, line + 1)(Some(0))
            case '\t' if tabprefix.isEmpty => compute(cs, i + 1, 0, line)(Some(col))
            case '\t' => compute(cs, i + 1, col + 4 - ((col - 1) & 3), line)
            case _ => compute(cs, i + 1, col + 1, line)
        }
    }
    compute(cs)

    override def apply(ctx: Context): Unit = {
        val inputsz = ctx.inputsz
        val input = ctx.input
        val i = ctx.offset
        if (inputsz != i) {
            @tailrec def go(i: Int, j: Int): Unit = {
                if (j < sz) {
                    val c = cs(j)
                    if (i == inputsz || input(i) != c) {
                        ctx.offset = i
                        val (colAdjust, lineAdjust) = adjustAtIndex(j)
                        ctx.col = colAdjust(ctx.col)
                        ctx.line = lineAdjust(ctx.line)
                        ctx.fail(expected)
                    }
                    else go(i + 1, j + 1)
                }
                else {
                    val (colAdjust, lineAdjust) = adjustAtIndex(j)
                    ctx.col = colAdjust(ctx.col)
                    ctx.line = lineAdjust(ctx.line)
                    ctx.offset = i
                    ctx.pushAndContinue(x)
                }
            }
            go(i, 0)
        }
        else ctx.fail(expected)
    }
    override def toString: String = if (x.isInstanceOf[String] && (s eq x.asInstanceOf[String])) s"Str($s)" else s"StrPerform($s, $x)"
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

private [internal] final class Fail(msg: String, expected: UnsafeOption[String]) extends Instr {
    override def apply(ctx: Context): Unit = {
        ctx.fail(expected)
        ctx.raw ::= msg
    }
    override def toString: String = s"Fail($msg)"
}

private [internal] final class Unexpected(msg: String, expected: UnsafeOption[String]) extends Instr {
    override def apply(ctx: Context): Unit = ctx.fail(expected = expected, unexpected = msg)
    override def toString: String = s"Unexpected($msg)"
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

private [internal] object Attempt extends Instr {
    override def apply(ctx: Context): Unit = {
        // Remove the recovery input from the stack, it isn't needed anymore
        if (ctx.status eq Good) {
            ctx.states = ctx.states.tail
            ctx.handlers = ctx.handlers.tail
            ctx.inc()
        }
        // Pop input off head then fail to next handler
        else {
            ctx.restoreState()
            ctx.fail()
        }
    }
    override def toString: String = "Attempt"
}

private [internal] object Look extends Instr {
    override def apply(ctx: Context): Unit = {
        if (ctx.status eq Good) {
            ctx.restoreState()
            ctx.handlers = ctx.handlers.tail
            ctx.inc()
        }
        else {
            ctx.states = ctx.states.tail
            ctx.fail()
        }
    }
    override def toString: String = "Look"
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
    override def apply(ctx: Context): Unit = {
        if (ctx.offset != ctx.checkStack.head) ctx.fail()
        else {
            ctx.status = Good
            ctx.inc()
        }
        ctx.checkStack = ctx.checkStack.tail
    }
    override def toString: String = s"Catch"
}

// Position Extractors
private [internal] object Line extends Instr {
    override def apply(ctx: Context): Unit = ctx.pushAndContinue(ctx.line)
    override def toString: String = "Line"
}

private [internal] object Col extends Instr {
    override def apply(ctx: Context): Unit = ctx.pushAndContinue(ctx.col)
    override def toString: String = "Col"
}

// Register-Manipulators
private [internal] final class Get(v: Int) extends Instr {
    override def apply(ctx: Context): Unit = ctx.pushAndContinue(ctx.regs(v))
    override def toString: String = s"Get($v)"
}

private [internal] final class Put(v: Int) extends Instr {
    override def apply(ctx: Context): Unit = {
        ctx.copyOnWrite(v, ctx.stack.peekAndExchange(()))
        ctx.inc()
    }
    override def toString: String = s"Put($v)"
}

private [internal] final class Modify[S](v: Int, f: S => S) extends Instr {
    private [this] val g = f.asInstanceOf[Any => Any]
    override def apply(ctx: Context): Unit = {
        ctx.copyOnWrite(v, g(ctx.regs(v)))
        ctx.pushAndContinue(())
    }
    override def toString: String = s"Modify($v, f)"
}

private [internal] final class LocalEntry(v: Int) extends Instr {
    override def apply(ctx: Context): Unit = {
        ctx.saveState()
        // This will always cause a copy
        ctx.copyOnWrite(v, ctx.stack.upop())
        ctx.inc()
    }
    override def toString: String = s"LocalEntry($v)"
}

private [internal] final class LocalExit[S](v: Int) extends Instr {
    override def apply(ctx: Context): Unit = {
        if (ctx.status eq Good) {
            ctx.regs(v) = ctx.states.head.regs(v)
            ctx.inc()
        }
        else ctx.fail()
        ctx.states = ctx.states.tail
    }
    override def toString: String = s"LocalExit($v)"
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

// Companion Objects
private [internal] object CharTok {
    def apply(c: Char, expected: UnsafeOption[String]): Instr = new CharTok(c, c, expected)
}

private [internal] object StringTok {
    def apply(s: String, expected: UnsafeOption[String]): StringTok = new StringTok(s, s, expected)
}
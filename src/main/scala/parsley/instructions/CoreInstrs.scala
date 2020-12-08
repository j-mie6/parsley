package parsley.instructions

import Stack.{isEmpty, push}
import parsley.{ResizableArray, UnsafeOption}
import parsley.deepembedding.Parsley

import scala.annotation.tailrec
import scala.language.existentials

// Stack Manipulators
private [parsley] final class Push[A](x: A) extends Instr
{
    override def apply(ctx: Context): Unit =
    {
        ctx.stack.push(x)
        ctx.inc()
    }
    override def toString: String = s"Push($x)"
}

private [parsley] object Pop extends Instr
{
    override def apply(ctx: Context): Unit =
    {
        ctx.stack.pop_()
        ctx.inc()
    }
    override def toString: String = "Pop"
}

private [parsley] object Flip extends Instr
{
    override def apply(ctx: Context): Unit =
    {
        val y = ctx.stack.upeek
        ctx.stack.exchange(ctx.stack(1))
        ctx.stack(1) = y
        ctx.inc()
    }
    override def toString: String = "Flip"
}

// Primitives
private [parsley] class CharTok protected (protected final val c: Char, _expected: UnsafeOption[String]) extends Instr
{
    protected val expected: String = if (_expected == null) "\"" + c + "\"" else _expected
    protected final val ac: Any = c
    override def apply(ctx: Context): Unit =
    {
        if (ctx.moreInput && ctx.nextChar == c)
        {
                ctx.stack.push(ac)
                ctx.offset += 1
                ctx.col += 1
                ctx.inc()
        }
        else ctx.fail(expected)
    }
    override final def toString: String = s"Chr($c)"
}

private [parsley] final class Satisfies(f: Char => Boolean, expected: UnsafeOption[String]) extends Instr
{
    override def apply(ctx: Context): Unit =
    {
        if (ctx.moreInput)
        {
            val c = ctx.nextChar
            if (f(ctx.nextChar))
            {
                ctx.stack.push(c)
                ctx.offset += 1
                c match
                {
                    case '\n' => ctx.line += 1; ctx.col = 1
                    case '\t' => ctx.col += 4 - ((ctx.col - 1) & 3)
                    case _ => ctx.col += 1
                }
                ctx.inc()
            }
            else ctx.fail(expected)
        }
        else ctx.fail(expected)
    }
    override def toString: String = "Sat(?)"
}

private [parsley] final class StringTok(s: String, _expected: UnsafeOption[String]) extends Instr
{
    private [this] val expected = if (_expected == null) "\"" + s + "\"" else _expected
    private [this] val cs = s.toCharArray
    private [this] val sz = cs.length
    private [this] val adjustAtIndex = new Array[(Int => Int, Int => Int)](s.length + 1)
    def makeAdjusters(col: Int, line: Int, tabprefix: Option[Int]): (Int => Int, Int => Int) =
        if (line > 0) ((_: Int) => col, (x: Int) => x + line)
        else (tabprefix match
        {
            case Some(prefix) =>
                val outer = 4 + col + prefix
                val inner = prefix - 1
                (x: Int) => outer + x - ((x + inner) & 3)
            case None => (x: Int) => x + col
        }, (x: Int) => x)
    @tailrec def compute(cs: Array[Char], i: Int = 0, col: Int = 0, line: Int = 0)(implicit tabprefix: Option[Int] = None): Unit =
    {
        adjustAtIndex(i) = makeAdjusters(col, line, tabprefix)
        if (i < cs.length) cs(i) match
        {
            case '\n' => compute(cs, i+1, 1, line + 1)(Some(0))
            case '\t' if tabprefix.isEmpty => compute(cs, i+1, 0, line)(Some(col))
            case '\t' => compute(cs, i+1, col + 4 - ((col-1) & 3), line)
            case _ => compute(cs, i+1, col + 1, line)
        }
    }
    compute(cs)

    override def apply(ctx: Context): Unit =
    {
        val strsz = this.sz
        val inputsz = ctx.inputsz
        val input = ctx.input
        var i = ctx.offset
        var j = 0
        val cs = this.cs
        if (inputsz != i)
        {
            while (j < strsz)
            {
                val c = cs(j)
                if (i == inputsz || input(i) != c)
                {
                    ctx.offset = i
                    val (colAdjust, lineAdjust) = adjustAtIndex(j)
                    ctx.col = colAdjust(ctx.col)
                    ctx.line = lineAdjust(ctx.line)
                    ctx.fail(expected)
                    return
                }
                i += 1
                j += 1
            }
            val (colAdjust, lineAdjust) = adjustAtIndex(j)
            ctx.col = colAdjust(ctx.col)
            ctx.line = lineAdjust(ctx.line)
            ctx.offset = i
            ctx.stack.push(s)
            ctx.inc()
        }
        else ctx.fail(expected)
    }
    override def toString: String = s"Str($s)"
}

// Applicative Functors
private [parsley] object Apply extends Instr
{
    override def apply(ctx: Context): Unit =
    {
        val x = ctx.stack.upop()
        val f = ctx.stack.peek[Any => Any]
        ctx.stack.exchange(f(x))
        ctx.inc()
    }
    override def toString: String = "Apply"
}

// Monadic
private [parsley] final class DynCall[-A](f: A => Array[Instr], expected: UnsafeOption[String]) extends Instr
{
    private [DynCall] val g = f.asInstanceOf[Any => Array[Instr]]
    override def apply(ctx: Context): Unit =
    {
        ctx.calls = push(ctx.calls, new Frame(ctx.pc + 1, ctx.instrs))
        ctx.instrs = g(ctx.stack.upop())
        ctx.depth += 1
        ctx.pc = 0
        if (expected != null && ctx.errorOverride == null)
        {
            ctx.overrideDepth = ctx.depth
            ctx.errorOverride = expected
        }
    }
    override def toString: String = "DynCall(?)"
}

// Control Flow
private [parsley] final class Call(_instrs: =>Array[Instr], expected: UnsafeOption[String]) extends Instr
{
    private [Call] lazy val (instrs, pindices) = {
        val is = _instrs
        (is, Parsley.statefulIndices(is))
    }

    override def apply(ctx: Context): Unit =
    {
        ctx.calls = push(ctx.calls, new Frame(ctx.pc + 1, ctx.instrs))
        ctx.instrs = Parsley.stateSafeCopy(instrs, pindices)
        ctx.depth += 1
        if (expected != null && ctx.errorOverride == null)
        {
            ctx.overrideDepth = ctx.depth
            ctx.errorOverride = expected
        }
        ctx.pc = 0
    }
    override def toString: String = "Call"
}

private [parsley] final class GoSub(var label: Int, expected: UnsafeOption[String]) extends JumpInstr
{
    override def apply(ctx: Context): Unit =
    {
        ctx.calls = push(ctx.calls, new Frame(ctx.pc + 1, ctx.instrs))
        ctx.pc = label
        ctx.depth += 1
        if (expected != null && ctx.errorOverride == null)
        {
            ctx.overrideDepth = ctx.depth
            ctx.errorOverride = expected
        }
    }
    override def toString: String = s"GoSub($label)"
}

private [parsley] object Return extends Instr
{
    override def apply(ctx: Context): Unit = ctx.ret()
    override def toString: String = "Return"
}

private [parsley] final class Fail(msg: String, expected: UnsafeOption[String]) extends Instr
{
    override def apply(ctx: Context): Unit =
    {
        ctx.fail(expected)
        ctx.raw ::= msg
    }
    override def toString: String = s"Fail($msg)"
}

private [parsley] final class Unexpected(msg: String, expected: UnsafeOption[String]) extends Instr
{
    override def apply(ctx: Context): Unit =
    {
        ctx.fail(expected)
        ctx.unexpected = msg
        ctx.unexpectAnyway = true
    }
    override def toString: String = s"Unexpected($msg)"
}

private [parsley] final class Empty(expected: UnsafeOption[String]) extends Instr
{
    override def apply(ctx: Context): Unit =
    {
        val strip = ctx.expected.isEmpty
        ctx.fail(expected)
        if (strip) ctx.unexpected = null
    }
    override def toString: String = "Empty"
}

private [parsley] final class PushHandler(var label: Int) extends JumpInstr
{
    override def apply(ctx: Context): Unit =
    {
        ctx.handlers = push(ctx.handlers, new Handler(ctx.depth, label, ctx.stack.usize))
        ctx.states = push(ctx.states, new State(ctx.offset, ctx.line, ctx.col, ctx.regs))
        ctx.inc()
    }
    override def toString: String = s"PushHandler($label)"
}

private [parsley] final class PushFallthrough(var label: Int) extends JumpInstr
{
    override def apply(ctx: Context): Unit =
    {
        ctx.handlers = push(ctx.handlers, new Handler(ctx.depth, label, ctx.stack.usize))
        ctx.inc()
    }
    override def toString: String = s"PushFallthrough($label)"
}

private [parsley] object Attempt extends Instr
{
    override def apply(ctx: Context): Unit =
    {
        // Remove the recovery input from the stack, it isn't needed anymore
        if (ctx.status eq Good)
        {
            ctx.states = ctx.states.tail
            ctx.handlers = ctx.handlers.tail
            ctx.inc()
        }
        // Pop input off head then fail to next handler
        else
        {
            val state = ctx.states.head
            ctx.states = ctx.states.tail
            ctx.offset = state.offset
            ctx.line = state.line
            ctx.col = state.col
            ctx.regs = state.regs
            ctx.fail()
        }
    }
    override def toString: String = "Attempt"
}

private [parsley] object Look extends Instr
{
    override def apply(ctx: Context): Unit =
    {
        if (ctx.status eq Good)
        {
            val state = ctx.states.head
            ctx.states = ctx.states.tail
            ctx.offset = state.offset
            ctx.line = state.line
            ctx.col = state.col
            ctx.regs = state.regs
            ctx.handlers = ctx.handlers.tail
            ctx.inc()
        }
        else
        {
            ctx.states = ctx.states.tail
            ctx.fail()
        }
    }
    override def toString: String = "Look"
}

private [parsley] final class InputCheck(var label: Int) extends JumpInstr
{
    override def apply(ctx: Context): Unit =
    {
        ctx.checkStack = push(ctx.checkStack, ctx.offset)
        ctx.handlers = push(ctx.handlers, new Handler(ctx.depth, label, ctx.stack.usize))
        ctx.inc()
    }
    override def toString: String = s"InputCheck($label)"
}

private [parsley] final class Jump(var label: Int) extends JumpInstr
{
    override def apply(ctx: Context): Unit = ctx.pc = label
    override def toString: String = s"Jump($label)"
}

private [parsley] final class JumpGood(var label: Int) extends JumpInstr
{
    override def apply(ctx: Context): Unit =
    {
        ctx.handlers = ctx.handlers.tail
        ctx.checkStack = ctx.checkStack.tail
        ctx.pc = label
    }
    override def toString: String = s"JumpGood($label)"
}

private [parsley] object Catch extends Instr
{
    override def apply(ctx: Context): Unit =
    {
        if (ctx.offset != ctx.checkStack.head) ctx.fail()
        else
        {
            ctx.status = Good
            ctx.inc()
        }
        ctx.checkStack = ctx.checkStack.tail
    }
    override def toString: String = s"Catch"
}

// Position Extractors
private [parsley] object Line extends Instr
{
    override def apply(ctx: Context): Unit =
    {
        ctx.stack.push(ctx.line)
        ctx.inc()
    }
    override def toString: String = "Line"
}

private [parsley] object Col extends Instr
{
    override def apply(ctx: Context): Unit =
    {
        ctx.stack.push(ctx.col)
        ctx.inc()
    }
    override def toString: String = "Col"
}

// Register-Manipulators
private [parsley] final class Get(v: Int) extends Instr
{
    override def apply(ctx: Context): Unit =
    {
        ctx.stack.push(ctx.regs(v))
        ctx.inc()
    }
    override def toString: String = s"Get($v)"
}

private [parsley] final class Put(v: Int) extends Instr
{
    override def apply(ctx: Context): Unit =
    {
        if (!isEmpty(ctx.states) && (ctx.states.head.regs eq ctx.regs)) ctx.regs = ctx.regs.clone
        ctx.regs(v) = ctx.stack.peekAndExchange(())
        ctx.inc()
    }
    override def toString: String = s"Put($v)"
}

private [parsley] final class Modify[S](v: Int, f: S => S) extends Instr
{
    private [this] val g = f.asInstanceOf[Any => Any]
    override def apply(ctx: Context): Unit =
    {
        if (!isEmpty(ctx.states) && (ctx.states.head.regs eq ctx.regs)) ctx.regs = ctx.regs.clone
        ctx.regs(v) = g(ctx.regs(v))
        ctx.stack.push(())
        ctx.inc()
    }
    override def toString: String = s"Modify($v, f)"
}

private [parsley] final class LocalEntry(v: Int) extends Instr
{
    override def apply(ctx: Context): Unit =
    {
        ctx.states = push(ctx.states, new State(0, 0, 0, ctx.regs))
        ctx.regs = ctx.regs.clone
        ctx.regs(v) = ctx.stack.upop()
        ctx.inc()
    }
    override def toString: String = s"LocalEntry($v)"
}

private [parsley] final class LocalExit[S](v: Int) extends Instr
{
    override def apply(ctx: Context): Unit =
    {
        if (ctx.status eq Good)
        {
            val x = ctx.states.head.regs(v)
            ctx.states = ctx.states.tail
            ctx.regs(v) = x
            ctx.inc()
        }
        else
        {
            ctx.states = ctx.states.tail
            ctx.fail()
        }
    }
    override def toString: String = s"LocalExit($v)"
}

// Debugging Instructions
private [instructions] trait Logger
{
    val name: String
    final def preludeString(dir: Char, ctx: Context, ends: String = "") =
    {
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
    final def indent(ctx: Context) = " " * (ctx.debuglvl * 2)
}

private [parsley] final class LogBegin(var label: Int, val name: String, break: Boolean) extends JumpInstr with Logger
{
    override def apply(ctx: Context): Unit =
    {
        println(preludeString('>', ctx))
        if (break)
        {
            print(s"${indent(ctx)}{stack: ${ctx.stack.mkString(", ")}})\n" +
                  s"${indent(ctx)}{registers: ${ctx.regs.zipWithIndex.map{case (x, i) => s"r$i: $x"}.mkString("[", ", ", "])}")}}\n" +
                  s"${indent(ctx)}...")
            Console.in.read()
        }
        ctx.debuglvl += 1
        ctx.handlers = push(ctx.handlers, new Handler(ctx.depth, label, ctx.stack.usize))
        ctx.inc()
    }
    override def toString: String = s"LogBegin($label, $name)"
}

private [parsley] final class LogEnd(val name: String, break: Boolean) extends Instr with Logger
{
    override def apply(ctx: Context): Unit =
    {
        ctx.debuglvl -= 1
        val end = " " + (ctx.status match
        {
            case Good =>
                ctx.handlers = ctx.handlers.tail
                ctx.inc()
                Console.GREEN + "Good" + Console.RESET
            case Recover | Failed =>
                ctx.fail()
                Console.RED + "Fail" + Console.RESET
        })
        println(preludeString('<', ctx, end))
        if (break)
        {
            print(s"${indent(ctx)}{stack: ${ctx.stack.mkString(", ")}})\n" +
                  s"${indent(ctx)}{registers: ${ctx.regs.zipWithIndex.map{case (x, i) => s"r$i: $x"}.mkString("[", ", ", "])}")}}\n" +
                  s"${indent(ctx)}...")
            Console.in.read()
        }

    }
    override def toString: String = s"LogEnd($name)"
}

// Extractor Objects
private [parsley] object CharTok
{
    def apply(c: Char, expected: UnsafeOption[String]): CharTok = c match
    {
        case '\n' => new Newline(expected)
        case '\t' => new Tab(expected)
        case _ => new CharTok(c, expected)
    }
}
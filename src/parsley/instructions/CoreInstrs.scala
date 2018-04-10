package parsley.instructions

import Stack.push
import parsley.{Parsley, ResizableArray, UnsafeOption}

import scala.annotation.{switch, tailrec}
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
                (c: @switch) match
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
    private [this] val (colAdjust, lineAdjust) =
    {
        @tailrec def compute(cs: Array[Char], i: Int = 0, col: Int = 0, line: Int = 0)(implicit tabprefix: Option[Int] = None): (Int, Int, Option[Int]) =
        {
            if (i < cs.length) (cs(i): @switch) match
            {
                case '\n' => compute(cs, i+1, 1, line + 1)(Some(0))
                case '\t' if tabprefix.isEmpty => compute(cs, i+1, 0, line)(Some(col))
                case '\t' => compute(cs, i+1, col + 4 - ((col-1) & 3), line)
                case _ => compute(cs, i+1, col + 1, line)
            }
            else (col, line, tabprefix)
        }
        val (col, line, tabprefix) = compute(cs)
        if (line > 0) ((_: Int) => col, (x: Int) => x + line)
        else (tabprefix match
        {
            case Some(prefix) => 
                val outer = 4 + col + prefix
                val inner = prefix - 1
                (x: Int) => outer + x - ((x + inner) & 3)
            case None => (x: Int) => x + col
        }, (x: Int) => x)
    }
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
                    ctx.fail(expected)
                    return
                }
                i += 1
                j += 1
            }
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
private [parsley] final class DynSub[-A](f: A => Array[Instr], expected: UnsafeOption[String]) extends Instr
{
    private [DynSub] val g = f.asInstanceOf[Any => Array[Instr]]
    override def apply(ctx: Context): Unit =
    {
        ctx.calls = push(ctx.calls, new Frame(ctx.pc + 1, ctx.instrs))
        ctx.instrs = g(ctx.stack.pop())
        ctx.depth += 1
        ctx.pc = 0
        if (expected != null)
        {
            ctx.overrideDepth = ctx.depth
            ctx.errorOverride = expected
        }
    }
    override def toString: String = "DynSub(?)"
}

// Control Flow
private [parsley] final class Call(p: Parsley[_], expected: UnsafeOption[String]) extends Instr
{
    private [this] var instrs: UnsafeOption[Array[Instr]] = _
    private [this] var pindices: Array[Int] = _
    private [this] var nstateful: Int = _
    override def apply(ctx: Context): Unit =
    {
        ctx.calls = push(ctx.calls, new Frame(ctx.pc + 1, ctx.instrs))
        if (instrs == null)
        {
            instrs = p.instrs //Note: This line cannot be hoisted, otherwise it will infinite loop during codeGen!
            var i: Int = 0
            val buff: ResizableArray[Int] = new ResizableArray[Int]()
            while (i < instrs.length)
            {
                if (instrs(i).isInstanceOf[Stateful]) buff += i
                i += 1
            }
            pindices = buff.toArray
            nstateful = pindices.length
        }
        // If there are any stateful instructions they MUST be deep-copied along with this instructions array!
        if (nstateful != 0)
        {
            var i: Int = 0
            while (i < nstateful)
            {
                val j = pindices(i)
                instrs(j) = instrs(j).copy
                i += 1
            }
            ctx.instrs = instrs.clone
        }
        ctx.depth += 1
        if (expected != null)
        {
            ctx.overrideDepth = ctx.depth
            ctx.errorOverride = expected
        }
        ctx.pc = 0
    }
    override def toString: String = s"Call($p)"
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
        ctx.states = push(ctx.states, new State(ctx.offset, ctx.line, ctx.col))
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
            ctx.fail()
        }
    }
    override def toString: String = "Attempt"
}

// State is not preserved after lookahead, the position is reset etc
// This should be the behaviour of the below when State is augmented
// but ensure this is the case later!
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
        if (ctx.status eq Good)
        {
            ctx.handlers = ctx.handlers.tail
            ctx.pc = label
        }
        // If the head of input stack is not the same size as the head of check stack, we fail to next handler
        else if (ctx.offset != ctx.checkStack.head) ctx.fail()
        else
        {
            ctx.status = Good
            ctx.inc()
        }
        ctx.checkStack = ctx.checkStack.tail
    }
    override def toString: String = s"JumpGood($label)"
}

// Register-Manipulators
private [parsley] final class Get(v: Int) extends Instr
{
    override def apply(ctx: Context): Unit = ???
    override def toString: String = s"Get($v)"
}

private [parsley] final class Put(v: Int) extends Instr with NoPush
{
    override def apply(ctx: Context): Unit = ???
    override def toString: String = s"Put($v)"
}

// Extractor Objects
private [parsley] object CharTok
{
    def apply(c: Char, expected: UnsafeOption[String]): CharTok = (c: @switch) match
    {
        case '\n' => new Newline(expected)
        case '\t' => new Tab(expected)
        case _ => new CharTok(c, expected)
    }
}
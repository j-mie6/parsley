package parsley.internal.instructions

import Stack.{isEmpty, push}
import parsley.internal.ResizableArray
import parsley.internal.UnsafeOption

import scala.annotation.tailrec

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

    @tailrec private def go(ctx: Context, i: Int, j: Int): Unit = {
        if (j < sz && i < ctx.inputsz && ctx.input(i) == cs(j)) go(ctx, i + 1, j + 1)
        else {
            val (colAdjust, lineAdjust) = adjustAtIndex(j)
            ctx.col = colAdjust(ctx.col)
            ctx.line = lineAdjust(ctx.line)
            ctx.offset = i
            if (j < sz) ctx.fail(expected)
            else ctx.pushAndContinue(x)
        }
    }

    override def apply(ctx: Context): Unit = go(ctx, ctx.offset, 0)
    override def toString: String = if (x.isInstanceOf[String] && (s eq x.asInstanceOf[String])) s"Str($s)" else s"StrPerform($s, $x)"
}

private [internal] final class Fail(msg: String, expected: UnsafeOption[String]) extends Instr {
    override def apply(ctx: Context): Unit = ctx.failWithMessage(expected, msg)
    override def toString: String = s"Fail($msg)"
}

private [internal] final class Unexpected(msg: String, expected: UnsafeOption[String]) extends Instr {
    override def apply(ctx: Context): Unit = ctx.unexpectedFail(expected = expected, unexpected = msg)
    override def toString: String = s"Unexpected($msg)"
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

// Companion Objects
private [internal] object CharTok {
    def apply(c: Char, expected: UnsafeOption[String]): Instr = new CharTok(c, c, expected)
}

private [internal] object StringTok {
    def apply(s: String, expected: UnsafeOption[String]): StringTok = new StringTok(s, s, expected)
}
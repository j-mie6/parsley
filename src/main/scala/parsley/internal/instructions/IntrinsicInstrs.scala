package parsley.internal.instructions

import parsley.internal.UnsafeOption
import Stack.isEmpty

import scala.annotation.tailrec

private [internal] final class Lift2[A, B, C](f: (A, B) => C) extends Instr {
    private [this] val g = f.asInstanceOf[(Any, Any) => C]
    override def apply(ctx: Context): Unit = {
        val y = ctx.stack.upop()
        ctx.exchangeAndContinue(g(ctx.stack.peek, y))
    }
    // $COVERAGE-OFF$
    override def toString: String = "Lift2(f)"
    // $COVERAGE-ON$
}

private [internal] final class Lift3[A, B, C, D](f: (A, B, C) => D) extends Instr {
    private [this] val g = f.asInstanceOf[(Any, Any, Any) => D]
    override def apply(ctx: Context): Unit = {
        val z = ctx.stack.upop()
        val y = ctx.stack.upop()
        ctx.exchangeAndContinue(g(ctx.stack.peek, y, z))
    }
    // $COVERAGE-OFF$
    override def toString: String = "Lift3(f)"
    // $COVERAGE-ON$
}

private [internal] class CharTok(c: Char, x: Any, _expected: UnsafeOption[String]) extends Instr {
    val expected: String = if (_expected == null) "\"" + c + "\"" else _expected
    override def apply(ctx: Context): Unit = {
        if (ctx.moreInput && ctx.nextChar == c) {
            ctx.consumeChar()
            ctx.pushAndContinue(x)
        }
        else ctx.fail(expected)
    }
    // $COVERAGE-OFF$
    override def toString: String = if (x == c) s"Chr($c)" else s"ChrPerform($c, $x)"
    // $COVERAGE-ON$
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
    // $COVERAGE-OFF$
    override def toString: String = if (x.isInstanceOf[String] && (s eq x.asInstanceOf[String])) s"Str($s)" else s"StrPerform($s, $x)"
    // $COVERAGE-ON$
}

private [internal] final class If(var label: Int) extends JumpInstr {
    override def apply(ctx: Context): Unit = {
        if (ctx.stack.pop()) ctx.pc = label
        else ctx.inc()
    }
    // $COVERAGE-OFF$
    override def toString: String = s"If(true: $label)"
    // $COVERAGE-ON$
}

private [internal] final class Filter[A](pred: A=>Boolean, expected: UnsafeOption[String]) extends Instr {
    private [this] val pred_ = pred.asInstanceOf[Any=>Boolean]
    override def apply(ctx: Context): Unit = {
        if (pred_(ctx.stack.upeek)) ctx.inc()
        else {
            val strip = ctx.expected.isEmpty
            ctx.fail(expected)
            if (strip) ctx.unexpected = null
        }
    }
    // $COVERAGE-OFF$
    override def toString: String = "Filter(?)"
    // $COVERAGE-ON$
}

private [internal] final class Guard[A](pred: A=>Boolean, msg: String, expected: UnsafeOption[String]) extends Instr {
    private [this] val pred_ = pred.asInstanceOf[Any=>Boolean]
    override def apply(ctx: Context): Unit = {
        if (pred_(ctx.stack.upeek)) ctx.inc()
        else ctx.failWithMessage(expected, msg)
    }
    // $COVERAGE-OFF$
    override def toString: String = s"Guard(?, $msg)"
    // $COVERAGE-ON$
}

private [internal] final class FastGuard[A](pred: A=>Boolean, msggen: A=>String, expected: UnsafeOption[String]) extends Instr {
    private [this] val pred_ = pred.asInstanceOf[Any=>Boolean]
    private [this] val msggen_ = msggen.asInstanceOf[Any=>String]
    override def apply(ctx: Context): Unit = {
        if (pred_(ctx.stack.upeek)) ctx.inc()
        else ctx.failWithMessage(expected, msggen_(ctx.stack.upop()))
    }
    // $COVERAGE-OFF$
    override def toString: String = "FastGuard(?, ?)"
    // $COVERAGE-ON$
}

private [internal] final class FastFail[A](msggen: A=>String, expected: UnsafeOption[String]) extends Instr {
    private [this] val msggen_ = msggen.asInstanceOf[Any => String]
    override def apply(ctx: Context): Unit = ctx.failWithMessage(expected, msggen_(ctx.stack.upop()))
    // $COVERAGE-OFF$
    override def toString: String = "FastFail(?)"
    // $COVERAGE-ON$
}

private [internal] final class FastUnexpected[A](msggen: A=>String, expected: UnsafeOption[String]) extends Instr {
    private [this] val msggen_ = msggen.asInstanceOf[Any => String]
    override def apply(ctx: Context): Unit = ctx.unexpectedFail(expected = expected, unexpected = msggen_(ctx.stack.upop()))
    // $COVERAGE-OFF$
    override def toString: String = "FastUnexpected(?)"
    // $COVERAGE-ON$
}

private [internal] final class NotFollowedBy(expected: UnsafeOption[String]) extends Instr {
    override def apply(ctx: Context): Unit = {
        // Recover the previous state; notFollowedBy NEVER consumes input
        ctx.restoreState()
        // A previous success is a failure
        if (ctx.status eq Good) {
            ctx.handlers = ctx.handlers.tail
            ctx.unexpectedFail(expected = expected, unexpected = "\"" + ctx.stack.upop().toString + "\"")
        }
        // A failure is what we wanted
        else {
            ctx.status = Good
            ctx.pushAndContinue(())
        }
    }
    // $COVERAGE-OFF$
    override def toString: String = "NotFollowedBy"
    // $COVERAGE-ON$
}

private [internal] class Eof(_expected: UnsafeOption[String]) extends Instr {
    val expected: String = if (_expected == null) "end of input" else _expected
    override def apply(ctx: Context): Unit = if (ctx.offset == ctx.inputsz) ctx.pushAndContinue(()) else ctx.fail(expected)
    // $COVERAGE-OFF$
    override final def toString: String = "Eof"
    // $COVERAGE-ON$
}

private [internal] final class Modify[S](reg: Int, f: S => S) extends Instr {
    private [this] val g = f.asInstanceOf[Any => Any]
    override def apply(ctx: Context): Unit = {
        ctx.writeReg(reg, g(ctx.regs(reg)))
        ctx.pushAndContinue(())
    }
    // $COVERAGE-OFF$
    override def toString: String = s"Modify($reg, f)"
    // $COVERAGE-ON$
}

private [internal] final class Local(var label: Int, reg: Int) extends JumpInstr with Stateful {
    private var saved: AnyRef = _
    private var inUse = false

    private def save(ctx: Context): Unit = saved = ctx.regs(reg).asInstanceOf[AnyRef]

    private def restore(ctx: Context): Unit = {
        ctx.regs(reg) = saved
        saved = null
    }

    private def continue(ctx: Context): Unit = {
        if (ctx.status eq Good) ctx.inc()
        else ctx.fail()
    }

    override def apply(ctx: Context): Unit = {
        // Second-entry, restore and either inc or fail
        if (inUse) {
            restore(ctx)
            inUse = false
            continue(ctx)
        }
        // Entry for the first time, register as a handle, and jump
        else {
            save(ctx)
            ctx.writeReg(reg, ctx.stack.upop())
            inUse = true
            ctx.pc = label
        }
    }

    // $COVERAGE-OFF$
    override def toString: String = s"Local($label, $reg)"
    // $COVERAGE-ON$
    override def copy: Local = new Local(label, reg)
}

// Companion Objects
private [internal] object CharTok {
    def apply(c: Char, expected: UnsafeOption[String]): Instr = new CharTok(c, c, expected)
}

private [internal] object StringTok {
    def apply(s: String, expected: UnsafeOption[String]): StringTok = new StringTok(s, s, expected)
}
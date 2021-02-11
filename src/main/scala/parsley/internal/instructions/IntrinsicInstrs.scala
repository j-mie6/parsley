package parsley.internal.instructions

import parsley.internal.UnsafeOption
import Stack.isEmpty

import scala.annotation.tailrec

private [internal] final class Lift2[A, B, C](_f: (A, B) => C) extends Instr {
    private [this] val f = _f.asInstanceOf[(Any, Any) => C]
    override def apply(ctx: Context): Unit = {
        val y = ctx.stack.upop()
        ctx.exchangeAndContinue(f(ctx.stack.peek, y))
    }
    // $COVERAGE-OFF$
    override def toString: String = "Lift2(f)"
    // $COVERAGE-ON$
}

private [internal] final class Lift3[A, B, C, D](_f: (A, B, C) => D) extends Instr {
    private [this] val f = _f.asInstanceOf[(Any, Any, Any) => D]
    override def apply(ctx: Context): Unit = {
        val z = ctx.stack.upop()
        val y = ctx.stack.upop()
        ctx.exchangeAndContinue(f(ctx.stack.peek, y, z))
    }
    // $COVERAGE-OFF$
    override def toString: String = "Lift3(f)"
    // $COVERAGE-ON$
}

private [internal] class CharTok(c: Char, x: Any, _expected: UnsafeOption[String]) extends Instr {
    private val expected: String = if (_expected == null) "\"" + c + "\"" else _expected
    private val errorItem: ErrorItem = if (_expected == null) Raw(c) else Desc(expected)
    override def apply(ctx: Context): Unit = {
        if (ctx.moreInput && ctx.nextChar == c) {
            ctx.consumeChar()
            ctx.pushAndContinue(x)
        }
        else ctx.expectedFail(Set(errorItem), None)
    }
    // $COVERAGE-OFF$
    override def toString: String = if (x == c) s"Chr($c)" else s"ChrPerform($c, $x)"
    // $COVERAGE-ON$
}

private [internal] final class StringTok private [instructions] (s: String, x: Any, _expected: UnsafeOption[String]) extends Instr {
    private [this] val expected = if (_expected == null) "\"" + s + "\"" else _expected
    private [this] val errorItem: ErrorItem = if (_expected == null) Raw(s) else Desc(expected)
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

    @tailrec private def go(ctx: Context, i: Int, j: Int, err: =>TrivialError): Unit = {
        if (j < sz && i < ctx.inputsz && ctx.input(i) == cs(j)) go(ctx, i + 1, j + 1, err)
        else {
            val (colAdjust, lineAdjust) = adjustAtIndex(j)
            ctx.col = colAdjust(ctx.col)
            ctx.line = lineAdjust(ctx.line)
            ctx.offset = i
            if (j < sz) ctx.fail(err)
            else ctx.pushAndContinue(x)
        }
    }

    override def apply(ctx: Context): Unit = {
        val origOffset = ctx.offset
        val origLine = ctx.line
        val origCol = ctx.col
        go(ctx, ctx.offset, 0,
            TrivialError(origOffset, origLine, origCol,
                Some(if (ctx.inputsz > origOffset) Raw(ctx.input.slice(origOffset, Math.min(origOffset + sz, ctx.inputsz)).mkString) else EndOfInput),
                Set(errorItem), Set.empty
            ))
    }
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

private [internal] final class Filter[A](_pred: A=>Boolean, expected: UnsafeOption[String]) extends Instr {
    private [this] val pred = _pred.asInstanceOf[Any=>Boolean]
    override def apply(ctx: Context): Unit = {
        if (pred(ctx.stack.upeek)) ctx.inc()
        else ctx.fail(TrivialError(ctx.offset, ctx.line, ctx.col, None, if (expected == null) Set.empty else Set(Desc(expected)), Set.empty))
    }
    // $COVERAGE-OFF$
    override def toString: String = "Filter(?)"
    // $COVERAGE-ON$
}

private [internal] final class FilterOut[A](_pred: PartialFunction[A, String], expected: UnsafeOption[String]) extends Instr {
    private [this] val pred = _pred.asInstanceOf[PartialFunction[Any, String]]
    override def apply(ctx: Context): Unit = {
        if (pred.isDefinedAt(ctx.stack.upeek)) {
            val reason = pred(ctx.stack.upop())
            ctx.fail(TrivialError(ctx.offset, ctx.line, ctx.col, None, if (expected == null) Set.empty else Set(Desc(expected)), Set(reason)))
        }
        else ctx.inc()
    }
    // $COVERAGE-OFF$
    override def toString: String = "FilterOut(?)"
    // $COVERAGE-ON$
}

private [internal] final class GuardAgainst[A](_pred: PartialFunction[A, String]) extends Instr {
    private [this] val pred = _pred.asInstanceOf[PartialFunction[Any, String]]
    override def apply(ctx: Context): Unit = {
        if (pred.isDefinedAt(ctx.stack.upeek)) ctx.failWithMessage(pred(ctx.stack.upop()))
        else ctx.inc()
    }
    // $COVERAGE-OFF$
    override def toString: String = "GuardAgainst(?)"
    // $COVERAGE-ON$
}

private [internal] final class NotFollowedBy(expected: UnsafeOption[String]) extends Instr {
    override def apply(ctx: Context): Unit = {
        // Recover the previous state; notFollowedBy NEVER consumes input
        ctx.restoreState()
        ctx.restoreHints()
        // A previous success is a failure
        if (ctx.status eq Good) {
            ctx.handlers = ctx.handlers.tail
            ctx.unexpectedFail(expected = expected, unexpected = "\"" + ctx.stack.upop().toString + "\"")
        }
        // A failure is what we wanted
        else {
            ctx.status = Good
            ctx.errs = ctx.errs.tail
            ctx.pushAndContinue(())
        }
    }
    // $COVERAGE-OFF$
    override def toString: String = "NotFollowedBy"
    // $COVERAGE-ON$
}

private [internal] class Eof(_expected: UnsafeOption[String]) extends Instr {
    val expected: String = if (_expected == null) "end of input" else _expected
    override def apply(ctx: Context): Unit = {
        if (ctx.offset == ctx.inputsz) ctx.pushAndContinue(())
        else {
            ctx.expectedFail(Set[ErrorItem](if (_expected == null) EndOfInput else Desc(_expected)), None)
        }
    }
    // $COVERAGE-OFF$
    override final def toString: String = "Eof"
    // $COVERAGE-ON$
}

private [internal] final class Modify[S](reg: Int, _f: S => S) extends Instr {
    private [this] val f = _f.asInstanceOf[Any => Any]
    override def apply(ctx: Context): Unit = {
        ctx.writeReg(reg, f(ctx.regs(reg)))
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
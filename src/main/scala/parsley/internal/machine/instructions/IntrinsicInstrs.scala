package parsley.internal.machine.instructions

import parsley.internal.errors.{Desc, Raw, EndOfInput}
import parsley.internal.machine.{Context, Good}
import parsley.internal.machine.errors.{EmptyError, EmptyErrorWithReason}

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

private [internal] class CharTok(c: Char, x: Any, _expected: Option[String]) extends Instr {
    private [this] final val errorItem = Some(_expected match {
        case Some(e) => Desc(e)
        case None    => Raw(c)
    })
    override def apply(ctx: Context): Unit = {
        if (ctx.moreInput && ctx.nextChar == c) {
            ctx.consumeChar()
            ctx.pushAndContinue(x)
        }
        else ctx.expectedFail(errorItem)
    }
    // $COVERAGE-OFF$
    override def toString: String = if (x == c) s"Chr($c)" else s"ChrPerform($c, $x)"
    // $COVERAGE-ON$
}

private [internal] final class StringTok private [instructions] (s: String, x: Any, _expected: Option[String]) extends Instr {
    private [this] final val errorItem = Some(_expected match {
        case Some(e) => Desc(e)
        case None    => Raw(s)
    })
    private [this] val cs = s.toCharArray
    private [this] val sz = cs.length

    @tailrec private [this] def compute(i: Int, lineAdjust: Int, colAdjust: StringTok.Adjust): (Int => Int, Int => Int) = {
        if (i < cs.length) {
            val (partialLineAdjust, partialColAdjust) = build(lineAdjust, colAdjust)
            partialLineAdjusters(i) = partialLineAdjust
            partialColAdjusters(i) = partialColAdjust
            cs(i) match {
                case '\n' => compute(i + 1, lineAdjust + 1, new StringTok.Set)
                case '\t' => compute(i + 1, lineAdjust, colAdjust.tab)
                case _    => colAdjust.next; compute(i + 1, lineAdjust, colAdjust)
            }
        }
        else build(lineAdjust, colAdjust)
    }
    private [this] def build(lineAdjust: Int, colAdjust: StringTok.Adjust): (Int => Int, Int => Int) = {
        (if (lineAdjust == 0) line => line else _ + lineAdjust, colAdjust.toAdjuster)
    }
    private [this] val partialLineAdjusters = new Array[Int => Int](sz)
    private [this] val partialColAdjusters = new Array[Int => Int](sz)
    private [this] val (lineAdjust, colAdjust) = compute(0, 0, new StringTok.Offset)

    @tailrec private def go(ctx: Context, i: Int, j: Int): Unit = {
        if (j < sz && i < ctx.inputsz && ctx.input.charAt(i) == cs(j)) go(ctx, i + 1, j + 1)
        else if (j < sz) {
            // The offset, line and column haven't been edited yet, so are in the right place
            ctx.expectedTokenFail(errorItem, sz)
            ctx.offset = i
            // These help maintain a consistent internal state, this makes the debuggers
            // output less confusing in the string case in particular.
            ctx.col = partialColAdjusters(j)(ctx.col)
            ctx.line = partialLineAdjusters(j)(ctx.line)
        }
        else {
            ctx.col = colAdjust(ctx.col)
            ctx.line = lineAdjust(ctx.line)
            ctx.offset = i
            ctx.pushAndContinue(x)
        }
    }

    override def apply(ctx: Context): Unit = go(ctx, ctx.offset, 0)
    // $COVERAGE-OFF$
    override def toString: String = if (x.isInstanceOf[String] && (s eq x.asInstanceOf[String])) s"Str($s)" else s"StrPerform($s, $x)"
    // $COVERAGE-ON$
}

private [internal] final class If(var label: Int) extends InstrWithLabel {
    override def apply(ctx: Context): Unit = {
        if (ctx.stack.pop()) ctx.pc = label
        else ctx.inc()
    }
    // $COVERAGE-OFF$
    override def toString: String = s"If(true: $label)"
    // $COVERAGE-ON$
}

private [internal] final class Case(var label: Int) extends InstrWithLabel {
    override def apply(ctx: Context): Unit = ctx.stack.pop[Either[_, _]]() match {
        case Left(x) =>
            ctx.stack.push(x)
            ctx.pc = label
        case Right(y) => ctx.pushAndContinue(y)
    }
    // $COVERAGE-OFF$
    override def toString: String = s"Case(left: $label)"
    // $COVERAGE-ON$
}

private [internal] final class Filter[A](_pred: A=>Boolean) extends Instr {
    private [this] val pred = _pred.asInstanceOf[Any=>Boolean]
    override def apply(ctx: Context): Unit = {
        if (pred(ctx.stack.upeek)) ctx.inc()
        else ctx.fail(new EmptyError(ctx.offset, ctx.line, ctx.col))
    }
    // $COVERAGE-OFF$
    override def toString: String = "Filter(?)"
    // $COVERAGE-ON$
}

private [internal] final class FilterOut[A](_pred: PartialFunction[A, String]) extends Instr {
    private [this] val pred = _pred.asInstanceOf[PartialFunction[Any, String]]
    override def apply(ctx: Context): Unit = {
        if (pred.isDefinedAt(ctx.stack.upeek)) {
            val reason = pred(ctx.stack.upop())
            ctx.fail(new EmptyErrorWithReason(ctx.offset, ctx.line, ctx.col, reason))
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

private [internal] object NotFollowedBy extends Instr {
    override def apply(ctx: Context): Unit = {
        val reached = ctx.offset
        // Recover the previous state; notFollowedBy NEVER consumes input
        ctx.restoreState()
        ctx.restoreHints()
        // A previous success is a failure
        if (ctx.status eq Good) {
            ctx.handlers = ctx.handlers.tail
            ctx.expectedTokenFail(None, reached - ctx.offset)
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

private [internal] object Eof extends Instr {
    private [this] final val expected = Some(EndOfInput)
    override def apply(ctx: Context): Unit = {
        if (ctx.offset == ctx.inputsz) ctx.pushAndContinue(())
        else ctx.expectedFail(expected)
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

private [internal] final class Local(var label: Int, reg: Int) extends InstrWithLabel with Stateful {
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
    def apply(c: Char, expected: Option[String]): Instr = new CharTok(c, c, expected)
}

private [internal] object StringTok {
    def apply(s: String, expected: Option[String]): StringTok = new StringTok(s, s, expected)

    private [StringTok] abstract class Adjust {
        private [StringTok] def tab: Adjust
        private [StringTok] def next: Unit
        private [StringTok] def toAdjuster: Int => Int
    }
    // A line has been read, so any updates are fixed
    private [StringTok] class Set extends Adjust {
        private [this] var at = 1
        // Round up to the nearest multiple of 4 /+1/
        private [StringTok] def tab = { at = ((at + 3) & -4) | 1; this }
        private [StringTok] def next = at += 1
        private [StringTok] def toAdjuster = {
            val x = at // capture it now, so it doesn't need to hold the object later
            _ => x
        }
    }
    // No information about alignment: a line or a tab hasn't been read
    private [StringTok] class Offset extends Adjust {
        private [this] var by = 0
        private [StringTok] def tab = new OffsetAlignOffset(by)
        private [StringTok] def next = by += 1
        private [StringTok] def toAdjuster = {
            val x = by // capture it now, so it doesn't need to hold the object later
            if (x == 0) col => col else _ + x
        }
    }
    // A tab was read, and no lines, so we adjust first, then align, and work with an aligned value
    private [StringTok] class OffsetAlignOffset(firstBy: Int) extends Adjust {
        private [this] var thenBy = 0
        // Round up to nearest multiple of /4/ (offset from aligned, not real value)
        private [StringTok] def tab = { thenBy = (thenBy | 3) + 1; this }
        private [StringTok] def next = thenBy += 1
        // Round up to the nearest multiple of 4 /+1/
        private [StringTok] def toAdjuster = {
            val x = firstBy // provide an indirection to the object
            val y = thenBy  // capture it now, so it doesn't need to hold the object later
            col => (((col + x + 3) & -4) | 1) + y
        }
    }
}

private [internal] object CharTokFastPerform {
    def apply[A >: Char, B](c: Char, f: A => B, expected: Option[String]): CharTok = new CharTok(c, f(c), expected)
}

private [internal] object StringTokFastPerform {
    def apply(s: String, f: String => Any, expected: Option[String]): StringTok = new StringTok(s, f(s), expected)
}
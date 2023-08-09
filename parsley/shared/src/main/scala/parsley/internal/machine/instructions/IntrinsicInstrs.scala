/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.instructions

import scala.annotation.tailrec

import parsley.XAssert._

import parsley.token.errors.LabelConfig

import parsley.internal.errors.{EndOfInput, ExpectDesc, ExpectItem}
import parsley.internal.machine.Context
import parsley.internal.machine.XAssert._

private [internal] final class Lift2(f: (Any, Any) => Any) extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        val y = ctx.stack.upop()
        ctx.exchangeAndContinue(f(ctx.stack.upeek, y))
    }
    // $COVERAGE-OFF$
    override def toString: String = "Lift2(f)"
    // $COVERAGE-ON$
}
private [internal] object Lift2 {
    def apply[A, B, C](f: (A, B) => C): Lift2 = new Lift2(f.asInstanceOf[(Any, Any) => Any])
}

private [internal] final class Lift3(f: (Any, Any, Any) => Any) extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        val z = ctx.stack.upop()
        val y = ctx.stack.upop()
        ctx.exchangeAndContinue(f(ctx.stack.upeek, y, z))
    }
    // $COVERAGE-OFF$
    override def toString: String = "Lift3(f)"
    // $COVERAGE-ON$
}
private [internal] object Lift3 {
    def apply[A, B, C, D](f: (A, B, C) => D): Lift3 = new Lift3(f.asInstanceOf[(Any, Any, Any) => Any])
}

private [internal] class CharTok(c: Char, x: Any, errorItem: Iterable[ExpectItem]) extends Instr {
    def this(c: Char, x: Any, expected: LabelConfig) = this(c, x, expected.asExpectItems(s"$c"))
    def this(c: Char, expected: LabelConfig) = this(c, c, expected)
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        if (ctx.moreInput && ctx.peekChar == c) {
            ctx.consumeChar()
            ctx.pushAndContinue(x)
        }
        else ctx.expectedFail(errorItem, unexpectedWidth = 1)
    }
    // $COVERAGE-OFF$
    override def toString: String = if (x == c) s"Chr($c)" else s"ChrExchange($c, $x)"
    // $COVERAGE-ON$
}

private [internal] class SupplementaryCharTok(codepoint: Int, x: Any, errorItem: Iterable[ExpectItem]) extends Instr {
    def this(codepoint: Int, x: Any, expected: LabelConfig) = this(codepoint, x, expected.asExpectItems(Character.toChars(codepoint).mkString))
    def this(codepoint: Int, expected: LabelConfig) = this(codepoint, codepoint, expected)

    assert(Character.isSupplementaryCodePoint(codepoint), "SupplementaryCharTok should only be used for supplementary code points")
    val h = Character.highSurrogate(codepoint)
    val l = Character.lowSurrogate(codepoint)
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        if (ctx.moreInput(2) && ctx.peekChar(0) == h && ctx.peekChar(1) == l) {
            ctx.fastConsumeSupplementaryChar()
            ctx.pushAndContinue(x)
        }
        else ctx.expectedFail(errorItem, unexpectedWidth = 1)
    }
    // $COVERAGE-OFF$
    override def toString: String =
        if (x == codepoint) s"SupplementaryChr($h$l)"
        else s"SupplementaryChrExchange($h$l, $x)"
    // $COVERAGE-ON$
}

private [internal] final class StringTok(s: String, x: Any, errorItem: Iterable[ExpectItem]) extends Instr {
    def this(s: String, x: Any, expected: LabelConfig) = this(s, x,  expected.asExpectItems(s))
    def this(s: String, expected: LabelConfig) = this(s, s, expected)

    private [this] val sz = s.length
    private [this] val codePointLength = s.codePointCount(0, sz)

    @tailrec private [this] def compute(i: Int, lineAdjust: Int, colAdjust: StringTok.Adjust): (Int => Int, Int => Int) = {
        if (i < sz) {
            val (partialLineAdjust, partialColAdjust) = build(lineAdjust, colAdjust)
            partialLineAdjusters(i) = partialLineAdjust
            partialColAdjusters(i) = partialColAdjust
            s.charAt(i) match {
                case '\n' => compute(i + 1, lineAdjust + 1, new StringTok.Set)
                case '\t' => compute(i + 1, lineAdjust, colAdjust.tab)
                case _    => colAdjust.next(); compute(i + 1, lineAdjust, colAdjust)
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
        if (j < sz && i < ctx.inputsz && ctx.input.charAt(i) == s.charAt(j)) go(ctx, i + 1, j + 1)
        else if (j < sz) {
            // The offset, line and column haven't been edited yet, so are in the right place
            // FIXME: this might be a more appropriate way of capping off the demand for the error?
            ctx.expectedFail(errorItem, /*s.codePointCount(0, j+1)*/codePointLength)
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

    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        go(ctx, ctx.offset, 0)
    }
    // $COVERAGE-OFF$
    override def toString: String = if (x.isInstanceOf[String] && (s eq x.asInstanceOf[String])) s"Str($s)" else s"StrPerform($s, $x)"
    // $COVERAGE-ON$
}

private [internal] final class UniSat(f: Int => Boolean, expected: Iterable[ExpectDesc]) extends Instr {
    def this(f: Int => Boolean, expected: LabelConfig) = this(f, expected.asExpectDescs)
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        lazy val hc = ctx.peekChar(0)
        lazy val h = hc.toInt
        lazy val l = ctx.peekChar(1)
        lazy val c = Character.toCodePoint(hc, l)
        if (ctx.moreInput(2) && hc.isHighSurrogate && Character.isSurrogatePair(hc, l) && f(c)) {
            ctx.fastConsumeSupplementaryChar()
            ctx.pushAndContinue(c)
        }
        else if (ctx.moreInput && f(h)) {
            ctx.updatePos(hc)
            ctx.offset += 1
            ctx.pushAndContinue(h)
        }
        else ctx.expectedFail(expected, unexpectedWidth = 1)
    }
    // $COVERAGE-OFF$
    override def toString: String = "UniSat(?)"
    // $COVERAGE-ON$
}

private [internal] final class If(var label: Int) extends InstrWithLabel {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        if (ctx.stack.pop[Boolean]()) ctx.pc = label
        else ctx.inc()
    }
    // $COVERAGE-OFF$
    override def toString: String = s"If(true: $label)"
    // $COVERAGE-ON$
}

private [internal] final class Case(var label: Int) extends InstrWithLabel {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        ctx.stack.peek[Either[_, _]] match {
            case Left(x)  =>
                ctx.stack.exchange(x)
                ctx.pc = label
            case Right(y) => ctx.exchangeAndContinue(y)
        }
    }
    // $COVERAGE-OFF$
    override def toString: String = s"Case(left: $label)"
    // $COVERAGE-ON$
}

private [internal] object NegLookFail extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        val reached = ctx.offset
        // Recover the previous state; notFollowedBy NEVER consumes input
        ctx.restoreState()
        ctx.restoreHints()
        // A previous success is a failure
        ctx.handlers = ctx.handlers.tail
        ctx.expectedFail(None, reached - ctx.offset)
    }
    // $COVERAGE-OFF$
    override def toString: String = "NegLookFail"
    // $COVERAGE-ON$
}

private [internal] object NegLookGood extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureHandlerInstruction(ctx)
        // Recover the previous state; notFollowedBy NEVER consumes input
        ctx.restoreState()
        ctx.restoreHints()
        // A failure is what we wanted
        ctx.good = true
        ctx.errs = ctx.errs.tail
        ctx.pushAndContinue(())
    }
    // $COVERAGE-OFF$
    override def toString: String = "NegLookGood"
    // $COVERAGE-ON$
}

private [internal] object Eof extends Instr {
    private [this] final val expected = Some(EndOfInput)
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        if (ctx.offset == ctx.inputsz) ctx.pushAndContinue(())
        else ctx.expectedFail(expected, unexpectedWidth = 1)
    }
    // $COVERAGE-OFF$
    override final def toString: String = "Eof"
    // $COVERAGE-ON$
}

private [internal] final class Modify(reg: Int, f: Any => Any) extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        ctx.writeReg(reg, f(ctx.regs(reg)))
        ctx.pushAndContinue(())
    }
    // $COVERAGE-OFF$
    override def toString: String = s"Modify($reg, f)"
    // $COVERAGE-ON$
}
private [internal] object Modify {
    def apply[S](reg: Int, f: S => S): Modify = new Modify(reg, f.asInstanceOf[Any => Any])
}

private [internal] final class SwapAndPut(reg: Int) extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        ctx.writeReg(reg, ctx.stack.peekAndExchange(ctx.stack.upop()))
        ctx.inc()
    }
    // $COVERAGE-OFF$
    override def toString: String = s"SwapAndPut(r$reg)"
    // $COVERAGE-ON$
}

private [internal] final class Filter[A](_pred: A => Boolean, var good: Int, var bad: Int) extends Instr {
    private [this] val pred = _pred.asInstanceOf[Any => Boolean]

    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        val x = ctx.stack.upeek
        if (pred(x)) {
            ctx.states = ctx.states.tail
            ctx.handlers = ctx.handlers.tail
            ctx.pc = good
        }
        else {
            ctx.handlers.pc = bad
            ctx.exchangeAndContinue((x, ctx.offset - ctx.states.offset))
        }
    }

    override def relabel(labels: Array[Int]): this.type = {
        good = labels(good)
        bad = labels(bad)
        this
    }
    // $COVERAGE-OFF$
    override def toString: String = s"Filter(???, good = $good)"
    // $COVERAGE-ON$
}

private [internal] final class MapFilter[A, B](_pred: A => Option[B], var good: Int, var bad: Int) extends Instr {
    private [this] val pred = _pred.asInstanceOf[Any => Option[B]]

    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        val x = ctx.stack.upeek
        val opt = pred(x)
        // Sorry, it's faster :(
        if (opt.isDefined) {
            ctx.states = ctx.states.tail
            ctx.handlers = ctx.handlers.tail
            ctx.stack.exchange(opt.get)
            ctx.pc = good
        }
        else {
            ctx.handlers.pc = bad
            ctx.exchangeAndContinue((x, ctx.offset - ctx.states.offset))
        }
    }

    override def relabel(labels: Array[Int]): this.type = {
        good = labels(good)
        bad = labels(bad)
        this
    }
    // $COVERAGE-OFF$
    override def toString: String = s"Filter(???, good = $good)"
    // $COVERAGE-ON$
}

// Companion Objects
private [internal] object StringTok {
    private [StringTok] abstract class Adjust {
        private [StringTok] def tab: Adjust
        private [StringTok] def next(): Unit
        private [StringTok] def toAdjuster: Int => Int
    }
    // A line has been read, so any updates are fixed
    private [StringTok] class Set extends Adjust {
        private [this] var at = 1
        // Round up to the nearest multiple of 4 /+1/
        private [StringTok] def tab = { at = ((at + 3) & -4) | 1; this } // scalastyle:ignore magic.number
        private [StringTok] def next() = at += 1
        private [StringTok] def toAdjuster = {
            val x = at // capture it now, so it doesn't need to hold the object later
            _ => x
        }
    }
    // No information about alignment: a line or a tab hasn't been read
    private [StringTok] class Offset extends Adjust {
        private [this] var by = 0
        private [StringTok] def tab = new OffsetAlignOffset(by)
        private [StringTok] def next() = by += 1
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
        private [StringTok] def next() = thenBy += 1
        // Round up to the nearest multiple of 4 /+1/
        private [StringTok] def toAdjuster = {
            val x = firstBy // provide an indirection to the object
            val y = thenBy  // capture it now, so it doesn't need to hold the object later
            col => (((col + x + 3) & -4) | 1) + y // scalastyle:ignore magic.number
        }
    }
}

private [internal] object CharTokFastPerform {
    def apply[A >: Char, B](c: Char, f: A => B, expected: LabelConfig): CharTok = new CharTok(c, f(c), expected)
}

private [internal] object SupplementaryCharTokFastPerform {
    def apply[A >: Int, B](c: Int, f: A => B, expected: LabelConfig): SupplementaryCharTok = new SupplementaryCharTok(c, f(c), expected)
}

private [internal] object StringTokFastPerform {
    def apply(s: String, f: String => Any, expected: LabelConfig): StringTok = new StringTok(s, f(s), expected)
}

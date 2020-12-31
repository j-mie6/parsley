package parsley.internal.instructions

import parsley.internal.UnsafeOption
import parsley.XCompat._
import Stack.push

import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.collection.mutable

private [internal] final class Perform[-A, +B](f: A => B) extends Instr {
    private [Perform] val g = f.asInstanceOf[Any => B]
    override def apply(ctx: Context): Unit = ctx.exchangeAndContinue(g(ctx.stack.upeek))
    override def toString: String = "Perform(?)"
}

private [internal] final class Exchange[A](private [Exchange] val x: A) extends Instr {
    override def apply(ctx: Context): Unit = ctx.exchangeAndContinue(x)
    override def toString: String = s"Ex($x)"
}

private [internal] final class SatisfyExchange[A](f: Char => Boolean, x: A, expected: UnsafeOption[String]) extends Instr {
    override def apply(ctx: Context): Unit = {
        if (ctx.moreInput && f(ctx.nextChar)) {
            ctx.consumeChar()
            ctx.pushAndContinue(x)
        }
        else ctx.fail(expected)
    }
    override def toString: String = s"SatEx(?, $x)"
}

private [internal] final class JumpGoodAttempt(var label: Int) extends JumpInstr {
    override def apply(ctx: Context): Unit = {
        if (ctx.status eq Good) {
            ctx.states = ctx.states.tail
            ctx.handlers = ctx.handlers.tail
            ctx.pc = label
        }
        else {
            ctx.restoreState()
            ctx.status = Good
            ctx.inc()
        }
    }
    override def toString: String = s"JumpGood'($label)"
}

private [internal] final class RecoverWith[A](x: A) extends Instr {
    override def apply(ctx: Context): Unit = ctx.catchNoConsumed {
        ctx.pushAndContinue(x)
    }
    override def toString: String = s"Recover($x)"
}

private [internal] final class AlwaysRecoverWith[A](x: A) extends Instr {
    override def apply(ctx: Context): Unit = {
        if (ctx.status eq Good) {
            ctx.states = ctx.states.tail
            ctx.handlers = ctx.handlers.tail
            ctx.inc()
        }
        else {
            ctx.restoreState()
            ctx.status = Good
            ctx.pushAndContinue(x)
        }
    }
    override def toString: String = s"AlwaysRecover($x)"
}

private [internal] final class JumpTable(prefixes: List[Char], labels: List[Int], private [this] var default: Int, _expecteds: List[UnsafeOption[String]])
    extends Instr {
    private [this] var defaultPreamble: Int = _
    private [this] val jumpTable = mutable.LongMap(prefixes.map(_.toLong).zip(labels): _*)
    val expecteds = prefixes.zip(_expecteds).map{case (c, expected) => if (expected == null) "\"" + c + "\"" else expected}

    override def apply(ctx: Context): Unit = {
        if (ctx.moreInput) {
            val dest = jumpTable.getOrElse(ctx.nextChar, default)
            ctx.pc = dest
            if (dest == default) addErrors(ctx)
            else {
                ctx.pushCheck()
                ctx.pushHandler(defaultPreamble)
            }
        }
        else {
            addErrors(ctx)
            ctx.pc = default
        }
    }

    private def addErrors(ctx: Context): Unit = {
        if (ctx.offset > ctx.erroffset) {
            ctx.erroffset = ctx.offset
            ctx.errcol = ctx.col
            ctx.errline = ctx.line
            ctx.unexpected = if (ctx.offset < ctx.inputsz) "\"" + ctx.nextChar + "\"" else "end of input"
            ctx.expected = if (ctx.errorOverride == null) expecteds else ctx.errorOverride::Nil
            ctx.raw = Nil
            ctx.unexpectAnyway = false
        }
        else if (ctx.offset == ctx.erroffset) {
            if (ctx.errorOverride == null) ctx.expected = ctx.expected reverse_::: expecteds
            else ctx.expected ::= ctx.errorOverride
        }
    }

    private [internal] def relabel(labels: Array[Int]): Unit = {
        jumpTable.mapValuesInPlace((_, v) => labels(v))
        default = labels(default)
        defaultPreamble = default - 1
    }
    override def toString: String = s"JumpTable(${jumpTable.map{case (k, v) => k.toChar -> v}.mkString(", ")}, _ -> $default)"
}

private [internal] object CharTokFastPerform {
    def apply[A >: Char, B](c: Char, f: A => B, expected: UnsafeOption[String]): CharTok = new CharTok(c, f(c), expected)
}

private [internal] object StringTokFastPerform {
    def apply(s: String, f: String => Any, expected: UnsafeOption[String]): StringTok = new StringTok(s, f(s), expected)
}

private [internal] object Exchange {
    def unapply[A](ex: Exchange[A]): Option[A] = Some(ex.x)
}

package parsley.internal.instructions

import parsley.internal.UnsafeOption
import parsley.XCompat._
import Stack.push

import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.collection.mutable

private [internal] final class Perform[-A, +B](_f: A => B) extends Instr {
    private [Perform] val f = _f.asInstanceOf[Any => B]
    override def apply(ctx: Context): Unit = ctx.exchangeAndContinue(f(ctx.stack.upeek))
    // $COVERAGE-OFF$
    override def toString: String = "Perform(?)"
    // $COVERAGE-ON$
}

private [internal] final class Exchange[A](private [Exchange] val x: A) extends Instr {
    override def apply(ctx: Context): Unit = ctx.exchangeAndContinue(x)
    // $COVERAGE-OFF$
    override def toString: String = s"Ex($x)"
    // $COVERAGE-ON$
}

private [internal] final class SatisfyExchange[A](f: Char => Boolean, x: A, expected: UnsafeOption[String]) extends Instr {
    override def apply(ctx: Context): Unit = {
        if (ctx.moreInput && f(ctx.nextChar)) {
            ctx.consumeChar()
            ctx.pushAndContinue(x)
        }
        else ctx.expectedFail(expected)
    }
    // $COVERAGE-OFF$
    override def toString: String = s"SatEx(?, $x)"
    // $COVERAGE-ON$
}

private [internal] final class JumpGoodAttempt(var label: Int) extends JumpInstr {
    override def apply(ctx: Context): Unit = {
        if (ctx.status eq Good) {
            ctx.states = ctx.states.tail
            ctx.handlers = ctx.handlers.tail
            ctx.commitHints()
            ctx.pc = label
        }
        else {
            ctx.restoreState()
            //ctx.addErrorToHints() //TODO: This actually does NOTHING?!
            ctx.restoreHints()
            ctx.status = Good
            ctx.inc()
        }
    }
    // $COVERAGE-OFF$
    override def toString: String = s"JumpGood'($label)"
    // $COVERAGE-ON$
}

private [internal] final class RecoverWith[A](x: A) extends Instr {
    override def apply(ctx: Context): Unit = {
        ctx.catchNoConsumed {
            ctx.addErrorToHintsAndPop()
            ctx.pushAndContinue(x)
        }
        ctx.restoreHints()
    }
    // $COVERAGE-OFF$
    override def toString: String = s"Recover($x)"
    // $COVERAGE-ON$
}

private [internal] final class AlwaysRecoverWith[A](x: A) extends Instr {
    override def apply(ctx: Context): Unit = {
        if (ctx.status eq Good) {
            ctx.states = ctx.states.tail
            ctx.handlers = ctx.handlers.tail
            ctx.commitHints()
            ctx.inc()
        }
        else {
            ctx.restoreState()
            ctx.addErrorToHintsAndPop()
            ctx.status = Good
            ctx.restoreHints()
            ctx.pushAndContinue(x)
        }
    }
    // $COVERAGE-OFF$
    override def toString: String = s"AlwaysRecover($x)"
    // $COVERAGE-ON$
}

private [internal] final class JumpTable(prefixes: List[Char], labels: List[Int], private [this] var default: Int, _expecteds: Map[Char, Set[ErrorItem]])
    extends Instr {
    private [this] var defaultPreamble: Int = _
    private [this] val jumpTable = mutable.LongMap(prefixes.map(_.toLong).zip(labels): _*)
    val expecteds = _expecteds.toList.flatMap(_._2.map(_.msg))
    val errorItems = _expecteds.toSet[(Char, Set[ErrorItem])].flatMap(_._2)

    override def apply(ctx: Context): Unit = {
        if (ctx.moreInput) {
            val dest = jumpTable.getOrElse(ctx.nextChar, default)
            ctx.pc = dest
            if (dest == default) addErrors(ctx)
            else {
                ctx.pushCheck()
                ctx.pushHandler(defaultPreamble)
                ctx.saveHints(shadow = false)
            }
        }
        else {
            addErrors(ctx)
            ctx.pc = default
        }
    }

    private def addErrors(ctx: Context): Unit = {
        val unexpected = if (ctx.offset < ctx.inputsz) Raw(s"${ctx.nextChar}") else EndOfInput
        // We need to save hints here so that the jump table does not get a chance to use the hints before it
        ctx.saveHints(shadow = false)
        ctx.pushError(TrivialError(ctx.offset, ctx.line, ctx.col, Some(unexpected), errorItems, Set.empty))
        ctx.restoreHints()
    }

    override def relabel(labels: Array[Int]): Unit = {
        jumpTable.mapValuesInPlace((_, v) => labels(v))
        default = labels(default)
        defaultPreamble = default - 1
    }
    // $COVERAGE-OFF$
    override def toString: String = s"JumpTable(${jumpTable.map{case (k, v) => k.toChar -> v}.mkString(", ")}, _ -> $default)"
    // $COVERAGE-ON$
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

package parsley.internal.instructions

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

private [internal] final class SatisfyExchange[A](f: Char => Boolean, x: A, _expected: Option[String]) extends Instr {
    private [this] final val expected: Set[ErrorItem] = _expected match {
        case Some(ex) => Set(Desc(ex))
        case None => Set.empty
    }
    override def apply(ctx: Context): Unit = {
        if (ctx.moreInput && f(ctx.nextChar)) {
            ctx.consumeChar()
            ctx.pushAndContinue(x)
        }
        else ctx.expectedFail(expected, reason = None)
    }
    // $COVERAGE-OFF$
    override def toString: String = s"SatEx(?, $x)"
    // $COVERAGE-ON$
}

private [internal] final class JumpGoodAttempt(var label: Int) extends InstrWithLabel {
    override def apply(ctx: Context): Unit = {
        if (ctx.status eq Good) {
            ctx.states = ctx.states.tail
            ctx.handlers = ctx.handlers.tail
            ctx.commitHints()
            ctx.pc = label
        }
        else {
            ctx.restoreState()
            ctx.restoreHints()
            ctx.status = Good
            ctx.inc()
        }
    }
    // $COVERAGE-OFF$
    override def toString: String = s"JumpGoodAttempt($label)"
    // $COVERAGE-ON$
}

private [internal] final class RecoverWith[A](x: A) extends Instr {
    override def apply(ctx: Context): Unit = {
        ctx.restoreHints() // This must be before adding the error to hints
        ctx.catchNoConsumed {
            ctx.addErrorToHintsAndPop()
            ctx.pushAndContinue(x)
        }
    }
    // $COVERAGE-OFF$
    override def toString: String = s"RecoverWith($x)"
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
            ctx.restoreHints() // This must be before adding the error to hints
            ctx.addErrorToHintsAndPop()
            ctx.status = Good
            ctx.pushAndContinue(x)
        }
    }
    // $COVERAGE-OFF$
    override def toString: String = s"AlwaysRecoverWith($x)"
    // $COVERAGE-ON$
}

private [internal] final class JumpTable(prefixes: List[Char], labels: List[Int],
        private [this] var default: Int,
        private [this] var merge: Int,
        _expecteds: Map[Char, Set[ErrorItem]]) extends Instr {
    private [this] var defaultPreamble: Int = _
    private [this] val jumpTable = mutable.LongMap(prefixes.map(_.toLong).zip(labels): _*)
    val errorItems = _expecteds.toSet[(Char, Set[ErrorItem])].flatMap(_._2)
    val messages = Set.empty[String]

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
        val unexpected = new Some(if (ctx.offset < ctx.inputsz) new Raw(s"${ctx.nextChar}") else EndOfInput)
        ctx.errs = push(ctx.errs, new TrivialError(ctx.offset, ctx.line, ctx.col, unexpected, errorItems, messages))
        ctx.pushHandler(merge)
    }

    override def relabel(labels: Array[Int]): this.type = {
        jumpTable.mapValuesInPlace((_, v) => labels(v))
        default = labels(default)
        merge = labels(merge)
        defaultPreamble = default - 1
        this
    }
    // $COVERAGE-OFF$
    override def toString: String = s"JumpTable(${jumpTable.map{case (k, v) => k.toChar -> v}.mkString(", ")}, _ -> $default, $merge)"
    // $COVERAGE-ON$
}
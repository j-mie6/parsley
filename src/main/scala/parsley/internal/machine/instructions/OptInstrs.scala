/* SPDX-FileCopyrightText: © 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.instructions

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.implicitConversions

import parsley.XCompat._

import parsley.internal.errors.{Desc, ErrorItem}
import parsley.internal.machine.{Context, Good}
import parsley.internal.machine.errors.MultiExpectedError
import parsley.internal.machine.stacks.ErrorStack

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
    private [this] final val expected = _expected.map(Desc(_))
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
        ctx.restoreState()
        ctx.restoreHints() // This must be before adding the error to hints
        ctx.addErrorToHintsAndPop()
        ctx.status = Good
        ctx.pushAndContinue(x)
    }
    // $COVERAGE-OFF$
    override def toString: String = s"AlwaysRecoverWith($x)"
    // $COVERAGE-ON$
}

private [internal] final class JumpTable(prefixes: List[Char], labels: List[Int],
        private [this] var default: Int,
        private [this] var merge: Int,
        private [this] val size: Int,
        private [this] val allErrorItems: Set[ErrorItem],
        private [this] val errorItemss: List[Set[ErrorItem]]) extends Instr {
    private [this] var defaultPreamble: Int = _
    private [this] val jumpTable = mutable.LongMap(prefixes.view.map(_.toLong).zip(labels.zip(errorItemss)).toSeq: _*)

    override def apply(ctx: Context): Unit = {
        if (ctx.moreInput) {
            val (dest, errorItems) = jumpTable.getOrElse(ctx.nextChar, (default, allErrorItems))
            ctx.pc = dest
            addErrors(ctx, errorItems) // adds a handler
            if (dest != default) {
                ctx.pushCheck()
                ctx.pushHandler(defaultPreamble)
                ctx.saveHints(shadow = false)
            }
        }
        else {
            addErrors(ctx, allErrorItems)
            ctx.pc = default
        }
    }

    private def addErrors(ctx: Context, errorItems: Set[ErrorItem]): Unit = {
        ctx.errs = new ErrorStack(new MultiExpectedError(ctx.offset, ctx.line, ctx.col, errorItems, size), ctx.errs)
        ctx.pushHandler(merge)
    }

    override def relabel(labels: Array[Int]): this.type = {
        jumpTable.mapValuesInPlace((_, v) => (labels(v._1), v._2))
        default = labels(default)
        merge = labels(merge)
        defaultPreamble = default - 1
        this
    }
    // $COVERAGE-OFF$
    override def toString: String = s"JumpTable(${jumpTable.map{case (k, v) => k.toChar -> v}.mkString(", ")}, _ -> $default, $merge)"
    // $COVERAGE-ON$
}

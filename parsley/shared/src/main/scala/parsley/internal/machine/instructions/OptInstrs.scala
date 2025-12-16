/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.instructions

import scala.collection.mutable

import parsley.XCompat._
import parsley.token.errors.LabelConfig

import parsley.internal.errors.ExpectItem
import parsley.internal.machine.Context
import parsley.internal.machine.XAssert._
import parsley.internal.machine.errors.{EmptyHints, ExpectedError}
import parsley.internal.machine.stacks.ErrorStack

private [internal] final class Lift1(f: Any => Any) extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        ctx.exchangeAndContinue(f(ctx.stack.upeek))
    }
    // $COVERAGE-OFF$
    override def toString: String = "Perform(?)"
    // $COVERAGE-ON$
}
private [internal] object Lift1 {
    def apply[A, B](f: A => B): Lift1 = new Lift1(f.asInstanceOf[Any => Any])
}

private [internal] final class Exchange[A](private [Exchange] val x: A) extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        ctx.exchangeAndContinue(x)
    }
    // $COVERAGE-OFF$
    override def toString: String = s"Ex($x)"
    // $COVERAGE-ON$
}

private [internal] final class SatisfyExchange[A](f: Char => Boolean, x: A, _expected: LabelConfig) extends Instr {
    private [this] final val expected = _expected.asExpectDescs
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        if (ctx.moreInput && f(ctx.peekChar)) {
            ctx.consumeChar()
            ctx.pushAndContinue(x)
        }
        else ctx.expectedFail(expected, unexpectedWidth = 1)
    }
    // $COVERAGE-OFF$
    override def toString: String = s"SatEx(?, $x)"
    // $COVERAGE-ON$
}

private [internal] final class RecoverWith[A](x: A) extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureHandlerInstruction(ctx)
        ctx.restoreHints() // This must be before adding the error to hints
        ctx.catchNoConsumed(ctx.handlers.check) {
            ctx.handlers = ctx.handlers.tail
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
        ensureHandlerInstruction(ctx)
        ctx.restoreState()
        ctx.restoreHints() // This must be before adding the error to hints
        ctx.handlers = ctx.handlers.tail
        ctx.addErrorToHintsAndPop()
        ctx.good = true
        ctx.pushAndContinue(x)
    }
    // $COVERAGE-OFF$
    override def toString: String = s"AlwaysRecoverWith($x)"
    // $COVERAGE-ON$
}

private [internal] case class JumpTablePredDef(val pred: Char => Boolean, var labelErrors: (Int, Iterable[ExpectItem]))

private [internal] final class JumpTable(jumpTable: List[Either[mutable.Map[Char, (Int, Iterable[ExpectItem])], JumpTablePredDef]],
        private [this] var default: Int,
        private [this] var merge: Int,
        size: Int,
        allErrorItems: Iterable[ExpectItem]) extends Instr {
    private [this] var defaultPreamble: Int = _
    private [this] var jumpTableFuncs: List[PartialFunction[Char, (Int, Iterable[ExpectItem])]] = _

    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        if (ctx.moreInput) {
            val (dest, errorItems) = getRoot(ctx.peekChar, jumpTableFuncs)
            ctx.pc = dest
            if (dest != default) {
                ctx.pushHandler(defaultPreamble)
                ctx.hints = EmptyHints
            }
            addErrors(ctx, errorItems) // adds a handler
        }
        else {
            addErrors(ctx, allErrorItems)
            ctx.pc = default
        }
    }

    // @tailrec
    private def getRoot(char: Char, fss: List[PartialFunction[Char, (Int, Iterable[ExpectItem])]]): (Int, Iterable[ExpectItem]) = fss match {
        // case f :: fs => if (f.isDefinedAt(char)) f(char) else getRoot(char, fs)
        case f :: fs => f.applyOrElse(char, getRoot(_, fs))
        case Nil     => (default, allErrorItems)
    }

    private def addErrors(ctx: Context, errorItems: Iterable[ExpectItem]): Unit = {
        // FIXME: the more appropriate way of demanding input may be to pick 1 character, for same rationale with StringTok
        ctx.errs = new ErrorStack(new ExpectedError(ctx.offset, ctx.line, ctx.col, errorItems, unexpectedWidth = size), ctx.errs)
        ctx.pushHandler(merge)
    }

    override def relabel(labels: Array[Int]): this.type = {
        jumpTable.foreach {
            case Left(map) => map.mapValuesInPlaceCompat {
                case (_, (i, errs)) => (labels(i), errs)
            }
            case Right(pred) => pred.labelErrors = (labels(pred.labelErrors._1), pred.labelErrors._2)
        }
        default = labels(default)
        merge = labels(merge)
        defaultPreamble = default - 1
        jumpTableFuncs = jumpTable.map {
            case Left(map) => map.toMap
            case Right(predDef) => {
                val pf: PartialFunction[Char, (Int, Iterable[ExpectItem])] = { case c: Char if predDef.pred(c) => predDef.labelErrors }
                pf
            }
        }
        this
    }

    private def tableToString: String = jumpTable.map {
        case Left(map) => s"${map.toList.sortBy{case (_, (l, _)) => l}.map{case (k, v) => s"${k.toChar} -> ${v._1}"}.mkString(", ")}"
        case Right(predDef) => s"?(_) -> ${predDef.labelErrors._1}"
    }.mkString(", ")
    // $COVERAGE-OFF$
    override def toString: String = s"JumpTable(${tableToString}, _ -> $default, $merge)"
    // $COVERAGE-ON$
}

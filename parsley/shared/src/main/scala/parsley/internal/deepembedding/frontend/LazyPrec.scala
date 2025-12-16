/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.frontend

import scala.annotation.tailrec
import scala.collection.mutable

import parsley.expr.{Fixity, Prec, Atoms, Level, Postfix, InfixL, InfixR, InfixN}

private [parsley] case class LazyOp(fixity: Fixity, op: LazyParsley[Any], prec: Int)

private [parsley] case class LazyPrec(atoms: List[LazyParsley[Any]], ops: List[LazyOp], wraps: List[Any => Any])

object LazyPrec {
    def apply(table: Prec[?]): LazyPrec = {
        val prec = fromPrec(table, level = 0, accOps = Nil, accWraps = mutable.ListBuffer.empty)
        val postfixOpPrecs = prec.ops.collect {
            case op if op.fixity == Postfix => op.prec
        }
        val infixOpPrecs = prec.ops.collect {
            case op if op.fixity == InfixL || op.fixity == InfixR || op.fixity == InfixN => op.prec
        }
        //postfixOpPrecs.minOption.lazyZip(infixOpPrecs.maxOption).forall(_ > _), but FIXME: 2.12 can't have nice things
        require(
            postfixOpPrecs.isEmpty || infixOpPrecs.isEmpty || postfixOpPrecs.min > infixOpPrecs.max,
            "Postfix operators may not have lower precedence than an infix operator"
        )
        prec
    }

    @tailrec
    private def fromPrec(table: Prec[?], level: Int, accOps: List[LazyOp], accWraps: mutable.ListBuffer[Any => Any]): LazyPrec = table match {
        case Atoms(atom0, atoms*) => LazyPrec((atom0 +: atoms).toList.map(_.internal), accOps, accWraps.toList)
        case Level(lower, ops) =>
            val newOps = ops.ops.map(op => LazyOp(ops.fixity, op.internal, level))
            fromPrec(lower, level + 1, newOps ++: accOps, accWraps += ops.wrap.asInstanceOf[Any => Any])
    }
}

/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.frontend

import scala.annotation.tailrec
import scala.collection.mutable

import parsley.expr.{Fixity, Prec, Atoms, Level, Postfix, InfixL, InfixR, InfixN}

import parsley.internal.deepembedding.backend, backend.StrictParsley
import parsley.internal.deepembedding.ContOps
import parsley.internal.deepembedding.ContOps.{suspend, ContAdapter}
import parsley.internal.deepembedding.Traverse.{traverse_, traverse}

private [parsley] final class LazyOp(val fixity: Fixity, val op: LazyParsley[Any], val prec: Int) {
    def preprocess[M[_, +_]: ContOps, R](implicit lets: LetMap): M[R, backend.StrictOp] =
        for (strictOp <- suspend(op.optimised[M, R, Any])) yield new backend.StrictOp(fixity, strictOp, prec)
}

private [parsley] final class Precedence[A](val atoms: List[LazyParsley[Any]], val ops: List[LazyOp], val wraps: List[Any => Any]) extends LazyParsley[A] {
    override def findLetsAux[M[_, +_]: ContOps, R](seen: Set[LazyParsley[?]])(implicit state: LetFinderState): M[R,Unit] =
        traverse_(atoms)(atom => suspend[M, R, Unit](atom.findLets(seen))) >>
        traverse_(ops)(op => suspend[M, R, Unit](op.op.findLets(seen)))

    override def preprocess[M[_, +_]: ContOps, R, A_ >: A](implicit lets: LetMap): M[R,StrictParsley[A_]] = for {
        atoms <- traverse(atoms)(_.optimised[M, R, Any])
        ops <- traverse(ops)(_.preprocess[M, R])
    } yield backend.Precedence(atoms, ops, wraps.toArray)

    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T,U], context: T): U[A] = visitor.visit(this, context)(atoms, ops, wraps)

    private [parsley] var debugName: String = "precedence"

}

private [parsley] object Precedence {
    def apply[A](table: Prec[A]): Precedence[A] = {
        val prec = fromPrec[A](table, level = 0, accOps = Nil, accWraps = mutable.ListBuffer.empty)
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
    private def fromPrec[A](table: Prec[?], level: Int, accOps: List[LazyOp], accWraps: mutable.ListBuffer[Any => Any]): Precedence[A] = table match {
        case Atoms(atom0, atoms*) => new Precedence((atom0 +: atoms).toList.map(_.internal), accOps, accWraps.toList)
        case Level(lower, ops) =>
            val newOps = ops.ops.map(op => new LazyOp(ops.fixity, op.internal, level))
            fromPrec[A](lower, level + 1, newOps ++: accOps, accWraps += ops.wrap.asInstanceOf[Any => Any])
    }
}

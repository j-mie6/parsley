/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.frontend

import parsley.internal.deepembedding.backend, backend.StrictParsley
import parsley.internal.deepembedding.ContOps
import parsley.internal.deepembedding.ContOps.{suspend, ContAdapter}
import parsley.internal.deepembedding.Traverse.{traverse_, traverse}

private [parsley] final class Precedence[A](table: LazyPrec) extends LazyParsley[A] {
  override protected def findLetsAux[M[_, +_]: ContOps, R](seen: Set[LazyParsley[_]])(implicit state: LetFinderState): M[R,Unit] = 
    traverse_(table.atoms)(a => suspend[M, R, Unit](a.findLets(seen))) >>
    traverse_(table.ops)(op => suspend[M, R, Unit](op.op.findLets(seen)))

  override protected def preprocess[M[_, +_]: ContOps, R, A_ >: A](implicit lets: LetMap): M[R,StrictParsley[A_]] = for {
    atoms <- traverse(table.atoms)(_.optimised[M, R, Any])
    ops <- traverse(table.ops)(op => for {
      strictOp <- suspend(op.op.optimised[M, R, Any])
    } yield backend.StrictOp(op.fixity, strictOp, op.prec))
  } yield {
    val strictPrec = backend.StrictPrec(atoms, ops, table.wraps.toArray)
    backend.Precedence(strictPrec)
  }

  override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T,U], context: T): U[A] = visitor.visit(this, context)(table)

  private [parsley] var debugName: String = "precedence"

}
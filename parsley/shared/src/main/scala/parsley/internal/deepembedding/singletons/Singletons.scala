/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.singletons

import parsley.internal.deepembedding.ContOps, ContOps.result
import parsley.internal.deepembedding.backend, backend.StrictParsley
import parsley.internal.deepembedding.frontend, frontend.LazyParsley
import parsley.internal.machine.instructions

/** Singletons are special instances of combinators which do not take other parsers as arguments.
  * As such, they cannot have recursive parsers inside them and do not need to distinguish between
  * strict and lazy combinator trees. They are also special in that they all generate single
  * instructions.
  *
  * The `Singleton` class captures these constraints to generalise all three of the frontend and
  * backend methods.
  *
  * @note due to the fact these appear in the frontend, they must not be mutable, for the same
  *       reasons as detailed in `LazyParsley`
  */
private [frontend] abstract class Singleton[A] extends LazyParsley[A] with StrictParsley[A] {
    /** The instruction that should be generated during the code generation for this combinator */
    def instr: instructions.Instr

    final override def inlinable: Boolean = true
    final override def findLetsAux[M[_, _]: ContOps, R](seen: Set[LazyParsley[_]])(implicit state: frontend.LetFinderState): M[R, Unit] = result(())
    final override def preprocess[M[_, _]: ContOps, R, A_ >: A](implicit lets: frontend.LetMap, recs: frontend.RecMap): M[R, StrictParsley[A_]] = {
        result(this)
    }
    final override def codeGen[M[_, _]: ContOps, R](implicit instrs: StrictParsley.InstrBuffer, state: backend.CodeGenState): M[R, Unit] = {
        result(instrs += instr)
    }
}

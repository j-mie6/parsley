/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.frontend

import parsley.debugger.objects.DebugContext

import parsley.internal.deepembedding.backend.StrictParsley
import parsley.internal.deepembedding.frontend

import scala.collection.mutable

package object debugger {
  private [parsley] def traverseDown[A]
    (parser: LazyParsley[A])
    (implicit seen: mutable.Set[LazyParsley[_]], dbgCtx: DebugContext): LazyParsley[A] =
  // This stops recursive parsers from causing an infinite recursion.
    if (seen.contains(parser)) {
      // Return a parser with a debugger attached.
      new Debugged(parser)
    } else {
      // Without this, we could potentially have infinite recursion from lazy-initialised parsers.
      seen.add(parser)

      // Function is buried in the frontend package to facilitate access to the GeneralisedEmbedding
      // abstract classes and their getters.
      getChildren(parser).map(traverseDown(_))

      // Return a parser with a debugger attached.
      new Debugged(reconstruct(parser))
    }

  // Attempt to retrieve the child parsers.
  private def getChildren(parser: LazyParsley[_]): List[LazyParsley[_]] =
    parser match {
      case p: frontend.Unary[_, _]         => List(p.parser)
      case p: frontend.Binary[_, _, _]     => List(p.leftParser, p.rightParser)
      case p: frontend.Ternary[_, _, _, _] => List(p.firstParser, p.secondParser, p.thirdParser)
      case p: frontend.<|>[_]              => List(p.leftParser, p.rightParser)
      case p: frontend.ChainPre[_]         => List(p.itemParser, p.opParser)
      case _ =>
        // This catches all atomic parsers (e.g. satisfy parsers).
        Nil
    }

  // Reconstruct the original parser with new components.
  // WARNING: very unsafe, use outside this object with caution.
  private def reconstruct[A, X, Y, Z]
    (parser: LazyParsley[A])
    (implicit dbgCtx: DebugContext): LazyParsley[A] =
    parser match {
      case par: frontend.Unary[X, A]         =>
        new frontend.Unary[X, A](new Debugged(par.parser)) {
          override def make(p: StrictParsley[X]): StrictParsley[A] = par.make(p)
        }
      case par: frontend.Binary[X, Y, A]     =>
        new frontend.Binary[X, Y, A](new Debugged(par.leftParser), new Debugged(par.rightParser)) {
          override def make(p: StrictParsley[X], q: StrictParsley[Y]): StrictParsley[A] = par.make(p, q)
        }
      case par: frontend.Ternary[X, Y, Z, A] =>
        new Ternary[X, Y, Z, A](new Debugged(par.firstParser), new Debugged(par.secondParser), new Debugged(par.thirdParser)) {
          override def make(p: StrictParsley[X], q: StrictParsley[Y], r: StrictParsley[Z]): StrictParsley[A] =
            par.make(p, q, r)
        }
      case par: frontend.<|>[A]              =>
        new frontend.<|>[A](new Debugged(par.leftParser), new Debugged(par.rightParser))
      case par: frontend.ChainPre[A]         =>
        new frontend.ChainPre[A](new Debugged(par.itemParser), new Debugged(par.opParser))
      case _                                 => parser
    }
}

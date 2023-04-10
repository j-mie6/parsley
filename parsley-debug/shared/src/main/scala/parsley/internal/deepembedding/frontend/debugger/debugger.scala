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
      val attached = getChildren(parser).map(traverseDown(_))

      // Return a parser with a debugger attached.
      new Debugged(reconstruct(parser, attached))
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
    (parser: LazyParsley[A], children: List[LazyParsley[Any]])
    (implicit dbgCtx: DebugContext): LazyParsley[A] = {
    // The children of a lot of n-ary parsers have different types, so this coercion is required
    // to make types match when reconstructing.
    def coerce[B](ix: Int): LazyParsley[B] =
      children(ix).asInstanceOf[LazyParsley[B]]

    parser match {
      case par: frontend.Unary[X, A]         =>
        new frontend.Unary[X, A](coerce[X](0)) {
          override def make(p: StrictParsley[X]): StrictParsley[A] = par.make(p)
        }
      case par: frontend.Binary[X, Y, A]     =>
        new frontend.Binary[X, Y, A](coerce[X](0), coerce[Y](1)) {
          override def make(p: StrictParsley[X], q: StrictParsley[Y]): StrictParsley[A] = par.make(p, q)
        }
      case par: frontend.Ternary[X, Y, Z, A] =>
        new Ternary[X, Y, Z, A](coerce[X](0), coerce[Y](1), coerce[Z](2)) {
          override def make(p: StrictParsley[X], q: StrictParsley[Y], r: StrictParsley[Z]): StrictParsley[A] =
            par.make(p, q, r)
        }
      case _: frontend.<|>[A]              =>
        new frontend.<|>[A](coerce[A](0), coerce[A](1))
      case _: frontend.ChainPre[A]         =>
        new frontend.ChainPre[A](coerce[A](0), coerce[A => A](1))
      case _                                 => parser
    }
  }


}

/* SPDX-FileCopyrightText: © 2023 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.frontend.debugger

import scala.collection.mutable

import parsley.debugger.internal.DebugContext

import parsley.internal.deepembedding.backend.StrictParsley
import parsley.internal.deepembedding.frontend.{<|>, >>=, Binary, ChainPre, GenericLazyParsley, GenericLazyParsleyIVisitor}
import parsley.internal.deepembedding.frontend.{LazyParsley, LazyParsleyIVisitor, Ternary, Unary}
import parsley.internal.deepembedding.singletons

object helpers {
  // This map tracks seen parsers to prevent infinitely recursive parsers from overflowing the stack.
  private [parsley] final class ParserTracker(val map: mutable.Map[LazyParsley[_], Debugged[_]]) extends AnyVal

  private [parsley] final class DebugInjectingVisitor(dbgCtx: DebugContext)
    extends GenericLazyParsleyIVisitor[ParserTracker, LazyParsley] {
    private def handlePossiblySeen[A](self: LazyParsley[A], context: ParserTracker)(dbgF: => LazyParsley[A]): Debugged[A] =
      if (context.map.contains(self)) {
        context.map(self).asInstanceOf[Debugged[A]]
      } else {
        val current = new Debugged[A](self, None, None)(dbgCtx)
        context.map.put(self, current)
        current.par = Some(dbgF)
        current
      }

    private def handleNoChildren[A](self: LazyParsley[A], context: ParserTracker): Debugged[A] =
      handlePossiblySeen[A](self, context)(self)

    override def visitSingleton[A](self: singletons.Singleton[A], context: ParserTracker): Debugged[A] = handleNoChildren(self, context)

    override def visitUnary[A, B](self: Unary[A, B], context: ParserTracker)(p: LazyParsley[A]): Debugged[B] =
      handlePossiblySeen[B](self, context) {
        val dbgC = p.visit(this, context)

        new Unary[A, B](dbgC) {
          override def make(p: StrictParsley[A]): StrictParsley[B] = self.make(p)

          override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[B] = visitor.visitGeneric(this, context)

          override private [parsley] def prettyName = self.prettyName
        }
      }

    override def visitBinary[A, B, C](self: Binary[A, B, C], context: ParserTracker)(l: LazyParsley[A], r: => LazyParsley[B]): Debugged[C] =
      handlePossiblySeen[C](self, context) {
        val dbgL = l.visit(this, context)
        val dbgR = r.visit(this, context)

        new Binary[A, B, C](dbgL, dbgR) {
          override def make(p: StrictParsley[A], q: StrictParsley[B]): StrictParsley[C] = self.make(p, q)

          override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[C] = visitor.visitGeneric(this, context)

          override private [parsley] def prettyName = self.prettyName
        }
      }

    override def visitTernary[A, B, C, D](self: Ternary[A, B, C, D], context: ParserTracker)(f: LazyParsley[A],
                                                                                             s: => LazyParsley[B],
                                                                                             t: => LazyParsley[C]): Debugged[D] =
      handlePossiblySeen[D](self, context) {
        val dbgF = f.visit(this, context)
        val dbgS = s.visit(this, context)
        val dbgT = t.visit(this, context)

        new Ternary[A, B, C, D](dbgF, dbgS, dbgT) {
          override def make(p: StrictParsley[A], q: StrictParsley[B], r: StrictParsley[C]): StrictParsley[D] = self.make(p, q, r)

          override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[D] = visitor.visitGeneric(this, context)

          override private [parsley] def prettyName = self.prettyName
        }
      }

    // We want flatMap-produced parsers to be debugged too, so we can see the full extent of the produced parse tree.
    // This is critical, as flatMap allows these parsers to be turing-complete, and can produce any arbitrary parse path.
    override def visit[A, B](self: A >>= B, context: ParserTracker)(p: LazyParsley[A], f: A => LazyParsley[B]): Debugged[B] =
      handlePossiblySeen[B](self, context) {
        val dbgC = p.visit(this, context)
        val dbgF = f andThen (_.visit(this, context))

        new >>=[A, B](dbgC, dbgF)
      }

    override def visit[A](self: <|>[A], context: ParserTracker)(p: LazyParsley[A], q: LazyParsley[A]): Debugged[A] =
      handlePossiblySeen[A](self, context) {
        val dbgL = p.visit(this, context)
        val dbgR = q.visit(this, context)

        new <|>[A](dbgL, dbgR)
      }

    override def visit[A](self: ChainPre[A], context: ParserTracker)(p: LazyParsley[A], op: => LazyParsley[A => A]): Debugged[A] =
      handlePossiblySeen[A](self, context) {
        val dbgP  = p.visit(this, context)
        val dbgOP = op.visit(this, context)

        new ChainPre[A](dbgP, dbgOP)
      }

    override def visitGeneric[A](self: GenericLazyParsley[A], context: ParserTracker): Debugged[A] = self match {
      case u: Unary[_, A]         => visitUnary(u, context)(u.p)
      case b: Binary[_, _, A]     => visitBinary(b, context)(b.left, b.right)
      case t: Ternary[_, _, _, A] => visitTernary(t, context)(t.first, t.second, t.third)
    }

    // XXX: This will assume all completely unknown parsers have no children at all (i.e. are Singletons).
    override def visitUnknown[A](self: LazyParsley[A], context: ParserTracker): Debugged[A] = self match {
      case d: Debugged[A] => d // No need to debug a parser twice!
      case n: Named[A]    => n.par match {
        case g: GenericLazyParsley[A] => visitGeneric(g, context).withName(n.name)
        case alt: <|>[A]              => alt.visit(this, context).asInstanceOf[Debugged[A]].withName(n.name)
        case cpre: ChainPre[A]        => cpre.visit(this, context).asInstanceOf[Debugged[A]].withName(n.name)
        case _                        => visitUnknown(n.par, context).withName(n.name)
      }
      case _              => handleNoChildren(self, context)
    }
  }
}

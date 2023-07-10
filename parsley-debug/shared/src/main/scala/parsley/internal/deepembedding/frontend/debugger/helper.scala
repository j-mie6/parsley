/* SPDX-FileCopyrightText: © 2023 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.frontend.debugger

import scala.collection.mutable

import org.typelevel.scalaccompat.annotation.unused
import parsley.debugger.internal.DebugContext
import parsley.internal.deepembedding.{singletons, Cont, ContOps}
import parsley.internal.deepembedding.ContOps.{perform, result, suspend, ContAdapter}
import parsley.internal.deepembedding.backend.StrictParsley
import parsley.internal.deepembedding.frontend.{<|>, >>=, Binary, ChainPre, GenericLazyParsley, GenericLazyParsleyIVisitor}
import parsley.internal.deepembedding.frontend.{LazyParsley, LazyParsleyIVisitor, Ternary, Unary}

private [parsley] object helper {
    // This map tracks seen parsers to prevent infinitely recursive parsers from overflowing the stack.
    // Use maps with weak keys or don't pass this into a >>= parser.
    private [parsley] final class ParserTracker(val map: mutable.Map[LazyParsley[_], Debugged[_]]) extends AnyVal

    // Keeping this around for easy access to LPM.
    @unused private [this] final class ContWrap[M[_, +_], R] {
        type LPM[+A] = M[R, LazyParsley[A]]
    }

    // XXX: Currently does not take into account the inherent stack safety of a parser being debugged
    //      in order to use the slightly faster Id route instead of Cont.
    private [parsley] def generateVisitorM[A](dbgCtx: DebugContext): DebugInjectingVisitorM[Cont.Impl, LazyParsley[A]] =
        new DebugInjectingVisitorM[Cont.Impl, LazyParsley[A]](dbgCtx)(Cont.ops)

    private [parsley] def visitWithM[A](parser: LazyParsley[A],
                                        tracker: ParserTracker,
                                        visitor: DebugInjectingVisitorM[Cont.Impl, LazyParsley[A]]): LazyParsley[A] =
        perform[Cont.Impl, LazyParsley[A]](parser.visit(visitor, tracker))(Cont.ops)

    // This visitor uses Cont / ContOps to ensure that if a parser is deeply recursive, the user can all a method
    // to use the trampoline ( https://en.wikipedia.org/wiki/Trampoline_(computing) ) to ensure that all calls are
    // turned into heap thunks instead of stack frames.
    private [parsley] final class DebugInjectingVisitorM[M[_, +_]: ContOps, R](dbgCtx: DebugContext)
        extends GenericLazyParsleyIVisitor[ParserTracker, ContWrap[M, R]#LPM] {
        private type L[+A] = ContWrap[M, R]#LPM[A]

        private def handlePossiblySeen[A](self: LazyParsley[A], context: ParserTracker)(dbgF: => L[A]): L[A] =
            if (context.map.contains(self)) {
                result(context.map(self).asInstanceOf[Debugged[A]])
            } else {
                dbgF.map { dbgF_ =>
                    val current = new Debugged[A](self, None, None)(dbgCtx)
                    context.map.put(self, current)
                    current.par = Some(dbgF_)
                    current
                }
            }

        private def handleNoChildren[A](self: LazyParsley[A], context: ParserTracker): L[A] =
            handlePossiblySeen[A](self, context)(result(self))

        override def visitSingleton[A](self: singletons.Singleton[A], context: ParserTracker): L[A] =
            handleNoChildren[A](self, context)

        override def visitUnary[A, B](self: Unary[A, B], context: ParserTracker)(p: LazyParsley[A]): L[B] =
            handlePossiblySeen[B](self, context) {
                for {
                    dbgC <- suspend(p.visit(this, context))
                } yield new Unary[A, B](dbgC) {
                    override def make(p: StrictParsley[A]): StrictParsley[B] = self.make(p)

                    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[B] = visitor.visitGeneric(this, context)

                    override private [parsley] def prettyName = self.prettyName
                }
            }

        override def visitBinary[A, B, C](self: Binary[A, B, C], context: ParserTracker)(l: LazyParsley[A], r: => LazyParsley[B]): L[C] =
            handlePossiblySeen[C](self, context) {
                for {
                    dbgL <- suspend(l.visit(this, context))
                    dbgR <- suspend(r.visit(this, context))
                } yield new Binary[A, B, C](dbgL, dbgR) {
                    override def make(p: StrictParsley[A], q: StrictParsley[B]): StrictParsley[C] = self.make(p, q)

                    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[C] = visitor.visitGeneric(this, context)

                    override private [parsley] def prettyName = self.prettyName
                }
            }

        override def visitTernary[A, B, C, D](self: Ternary[A, B, C, D], context: ParserTracker)(f: LazyParsley[A],
                                                                                                 s: => LazyParsley[B],
                                                                                                 t: => LazyParsley[C]): L[D] =
            handlePossiblySeen[D](self, context) {
                for {
                    dbgF <- suspend(f.visit(this, context))
                    dbgS <- suspend(s.visit(this, context))
                    dbgT <- suspend(t.visit(this, context))
                } yield new Ternary[A, B, C, D](dbgF, dbgS, dbgT) {
                    override def make(p: StrictParsley[A], q: StrictParsley[B], r: StrictParsley[C]): StrictParsley[D] = self.make(p, q, r)

                    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[D] = visitor.visitGeneric(this, context)

                    override private [parsley] def prettyName = self.prettyName
                }
            }

        // We want flatMap-produced parsers to be debugged too, so we can see the full extent of the produced parse tree.
        // This is critical, as flatMap allows these parsers to be turing-complete, and can produce any arbitrary parse path.
        override def visit[A, B](self: A >>= B, context: ParserTracker)(p: LazyParsley[A], f: A => LazyParsley[B]): L[B] =
            handlePossiblySeen[B](self, context) {
                // flatMap / >>= produces parsers arbitrarily, so there is no way we'd match by reference.
                // This is why a map with weak keys is required.
                for {
                    dbgC <- suspend(p.visit(this, context))
                } yield {
                    def dbgF(x: A): LazyParsley[B] = {
                        val subvisitor = new DebugInjectingVisitorM[M, LazyParsley[B]](dbgCtx)
                        perform[M, LazyParsley[B]](f(x).visit(subvisitor, context))
                    }
                    new >>=(dbgC, dbgF)
                }
            }

        override def visit[A](self: <|>[A], context: ParserTracker)(p: LazyParsley[A], q: LazyParsley[A]): L[A] =
            handlePossiblySeen[A](self, context) {
                for {
                    dbgP <- suspend(p.visit(this, context))
                    dbgQ <- suspend(q.visit(this, context))
                } yield new <|>(dbgP, dbgQ)
            }

        override def visit[A](self: ChainPre[A], context: ParserTracker)(p: LazyParsley[A], op: => LazyParsley[A => A]): L[A] =
            handlePossiblySeen[A](self, context) {
                for {
                    dbgP  <- suspend(p.visit(this, context))
                    dbgOp <- suspend(op.visit(this, context))
                } yield new ChainPre(dbgP, dbgOp)
            }

        // XXX: This will assume all completely unknown parsers have no children at all (i.e. are Singletons).
        override def visitUnknown[A](self: LazyParsley[A], context: ParserTracker): L[A] = self match {
            case d: Debugged[A] => result(d) // No need to debug a parser twice!
            case n: Named[A]    => n.par match {
                case g: GenericLazyParsley[A] => visitGeneric(g, context).map(_.asInstanceOf[Debugged[A]].withName(n.name))
                case alt: <|>[A]              => alt.visit(this, context).map(_.asInstanceOf[Debugged[A]].withName(n.name))
                case cpre: ChainPre[A]        => cpre.visit(this, context).map(_.asInstanceOf[Debugged[A]].withName(n.name))
                case _                        => visitUnknown(n.par, context).map(_.asInstanceOf[Debugged[A]].withName(n.name))
            }
            case _              => handleNoChildren(self, context)
        }
    }
}

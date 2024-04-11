/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.frontend.debugger

import scala.collection.{Factory, mutable}

import org.typelevel.scalaccompat.annotation.unused

import parsley.XAssert
import parsley.debugger.internal.XWeakMap

import parsley.internal.deepembedding.{singletons, Cont, ContOps, Id}
import parsley.internal.deepembedding.ContOps.{perform, result, suspend, zipWith, zipWith3, ContAdapter}
import parsley.internal.deepembedding.backend.StrictParsley
import parsley.internal.deepembedding.backend.debugger.DebugStrategy
import parsley.internal.deepembedding.frontend._ // scalastyle:ignore underscore.import

// Wrapper class signifying debugged classes
// TODO: the origin is needed to figure out the name later on... but couldn't we resolve the name here and avoid forwarding on to the backend (send string instead)?
// FIXME: this clobbers the register allocator, apparently?
private [parsley] final class TaggedWith[A](strat: DebugStrategy)(val origin: LazyParsley[A], var subParser: LazyParsley[A], userAssignedName: Option[String])
    extends LazyParsley[A] {
    XAssert.assert(!origin.isInstanceOf[TaggedWith[_]], "Tagged parsers should not be nested within each other directly.")

    def make(p: StrictParsley[A]): StrictParsley[A] = strat.create(origin, p, userAssignedName)

    override def findLetsAux[M[_, +_] : ContOps, R](seen: Set[LazyParsley[_]])(implicit state: LetFinderState): M[R, Unit] = suspend(subParser.findLets(seen))
    override def preprocess[M[_, +_] : ContOps, R, A_ >: A](implicit lets: LetMap): M[R, StrictParsley[A_]] = {
        for (p <- suspend[M, R, StrictParsley[A]](subParser.optimised[M, R, A])) yield make(p)
    }

    // $COVERAGE-OFF$
    private [frontend] def withName(name: String): TaggedWith[A] = new TaggedWith(strat)(origin, subParser, Some(name))
    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[A] = visitor.visitUnknown(this, context)
    // $COVERAGE-ON$

    override private [parsley] def prettyName = userAssignedName.getOrElse(origin.prettyName)
}

private [parsley] object TaggedWith {
    // Run this to inject the debugger itself.
    def tagRecursively[A](parser: LazyParsley[A], strategy: DebugStrategy): LazyParsley[A] = {
        // XXX: A weak map is needed so that memory leaks will not be caused by flatMap parsers.
        //      We want a decent amount of initial space to speed up debugging larger parsers slightly.
        val tracker: ParserTracker = new ParserTracker(new XWeakMap)
        if (parser.isCps) {
            implicit val ops: ContOps[Cont.Impl] = Cont.ops
            val visitor = new DebugInjectingVisitorM[Cont.Impl, LazyParsley[A]](strategy)
            visitWithM[Cont.Impl, A](parser, tracker, visitor)
        } else {
            implicit val ops: ContOps[Id.Impl] = Id.ops
            val visitor = new DebugInjectingVisitorM[Id.Impl, LazyParsley[A]](strategy)
            visitWithM[Id.Impl, A](parser, tracker, visitor)
        }
    }

    // This map tracks seen parsers to prevent infinitely recursive parsers from overflowing the stack (and ties
    // the knot for these recursive parsers).
    // Use maps with weak keys or don't pass this into a >>= parser.
    private final class ParserTracker(val map: mutable.Map[LazyParsley[_], TaggedWith[_]]) {
        def put[A](par: LazyParsley[A], dbg: TaggedWith[A]): Unit = map(par) = dbg
        def get[A](par: LazyParsley[A]): TaggedWith[A] = map(par).asInstanceOf[TaggedWith[A]]
        def contains(par: LazyParsley[_]): Boolean = map.contains(par)
    }

    // Keeping this around for easy access to LPM.
    @unused private [this] final class ContWrap[M[_, +_], R] {
        type LPM[+A] = M[R, LazyParsley[A]]
    }

    private def visitWithM[M[_, +_]: ContOps, A](parser: LazyParsley[A], tracker: ParserTracker, visitor: DebugInjectingVisitorM[M, LazyParsley[A]]) = {
        perform[M, LazyParsley[A]](parser.visit(visitor, tracker))
    }

    // This visitor uses Cont / ContOps to ensure that if a parser is deeply recursive, the user can all a method
    // to use the trampoline ( https://en.wikipedia.org/wiki/Trampoline_(computing) ) to ensure that all calls are
    // turned into heap thunks instead of stack frames.
    // $COVERAGE-OFF$
    private final class DebugInjectingVisitorM[M[_, +_]: ContOps, R](strategy: DebugStrategy)
        extends GenericLazyParsleyIVisitor[ParserTracker, ContWrap[M, R]#LPM] {
        private type L[+A] = ContWrap[M, R]#LPM[A]

        // This is the main logic for the visitor: everything else is just plumbing
        private def handlePossiblySeen[A](self: LazyParsley[A], context: ParserTracker)(subParser: =>L[A]): L[A] = {
            if (context.contains(self)) result(context.get(self))
            else {
                // let me guess, the map needs to be populated before the subParser traversal happens to resolve recursion...
                // TODO: any way around that? would get rid of the nasty `null` + mutvar
                val taggedSelf = new TaggedWith(strategy)(self, null, None)
                context.put(self, taggedSelf)
                subParser.map { subParser_ =>
                    // rewrite the tagged node to contain its processed sub-tree
                    taggedSelf.subParser = subParser_
                    taggedSelf
                }
            }
        }

        private def handleNoChildren[A](self: LazyParsley[A], context: ParserTracker): L[A] = handlePossiblySeen(self, context)(result(self))

        // We assume _q must be lazy, as it'd be better to *not* force a strict value versus accidentally forcing a lazy value.
        // This is called handle2Ary as to not be confused with handling Binary[_, _, _].
        private def handle2Ary[P[X] <: LazyParsley[X], A, B, C](self: P[A], context: ParserTracker)(p: LazyParsley[B], _q: =>LazyParsley[C])
                                                               (constructor: (LazyParsley[B], LazyParsley[C]) => P[A]): L[A] = {
            handlePossiblySeen[A](self, context) {
                zipWith(suspend[M, R, LazyParsley[B]](p.visit(this, context)), suspend[M, R, LazyParsley[C]](_q.visit(this, context)))(constructor)
            }
        }

        override def visitSingleton[A](self: singletons.Singleton[A], context: ParserTracker): L[A] = handleNoChildren[A](self, context)

        override def visitUnary[A, B](self: Unary[A, B], context: ParserTracker)(p: LazyParsley[A]): L[B] = handlePossiblySeen[B](self, context) {
            suspend[M, R, LazyParsley[A]](p.visit(this, context)).map { dbgC =>
                new Unary[A, B](dbgC) {
                    override def make(p: StrictParsley[A]): StrictParsley[B] = self.make(p)

                    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[B] = visitor.visitGeneric(this, context)

                    override private[parsley] def prettyName = self.prettyName
                }
            }
        }

        override def visitBinary[A, B, C](self: Binary[A, B, C], context: ParserTracker)(l: LazyParsley[A], r: =>LazyParsley[B]): L[C] = {
            handlePossiblySeen(self, context) {
                zipWith(suspend[M, R, LazyParsley[A]](l.visit(this, context)),
                        suspend[M, R, LazyParsley[B]](r.visit(this, context))) { (dbgL: LazyParsley[A], dbgR: LazyParsley[B]) =>
                    new Binary[A, B, C](dbgL, dbgR) {
                        override def make(p: StrictParsley[A], q: StrictParsley[B]): StrictParsley[C] = self.make(p, q)

                        override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[C] = visitor.visitGeneric(this, context)

                        override private [parsley] def prettyName = self.prettyName
                    }
                }
            }
        }

        override def visitTernary[A, B, C, D](self: Ternary[A, B, C, D], context: ParserTracker)
                                             (f: LazyParsley[A], s: =>LazyParsley[B], t: =>LazyParsley[C]): L[D] = handlePossiblySeen[D](self, context) {
            zipWith3(suspend[M, R, LazyParsley[A]](f.visit(this, context)),
                     suspend[M, R, LazyParsley[B]](s.visit(this, context)),
                     suspend[M, R, LazyParsley[C]](t.visit(this, context))) { (dbgF, dbgS, dbgT) =>
                new Ternary[A, B, C, D](dbgF, dbgS, dbgT) {
                    override def make(p: StrictParsley[A], q: StrictParsley[B], r: StrictParsley[C]): StrictParsley[D] = self.make(p, q, r)

                    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[D] = visitor.visitGeneric(this, context)

                    override private[parsley] def prettyName = self.prettyName
                }
            }
        }

        // We want flatMap-produced parsers to be debugged too, so we can see the full extent of the produced parse tree.
        // This is critical, as flatMap allows these parsers to be turing-complete, and can produce any arbitrary parse path.
        override def visit[A, B](self: A >>= B, context: ParserTracker)(p: LazyParsley[A], f: A => LazyParsley[B]): L[B] = {
            handlePossiblySeen(self, context) {
                // flatMap / >>= produces parsers arbitrarily, so there is no way we'd match by reference.
                // This is why a map with weak keys is required, so that these entries do not flood the map and
                // cause a massive memory leak.
                suspend[M, R, LazyParsley[A]](p.visit(this, context)).map { dbgC =>
                    def dbgF(x: A): LazyParsley[B] = {
                        val subvisitor = new DebugInjectingVisitorM[M, LazyParsley[B]](strategy)
                        perform[M, LazyParsley[B]](f(x).visit(subvisitor, context))
                    }
                    new >>=(dbgC, dbgF)
                }
            }
        }

        override def visit[A](self: <|>[A], context: ParserTracker)(p: LazyParsley[A], q: LazyParsley[A]): L[A] = handle2Ary(self, context)(p, q)(new <|>(_, _))

        // Iterative parsers need their own handling.
        override def visit[A, C](self: Many[A, C], context: ParserTracker)(p: LazyParsley[A], factory: Factory[A, C]): L[C] = {
            handlePossiblySeen(self, context) {
                suspend[M, R, LazyParsley[A]](p.visit(this, context)).map(new Many[A, C](_, factory))
            }
        }

        override def visit[A](self: ChainPost[A], context: ParserTracker)(p: LazyParsley[A], _op: =>LazyParsley[A => A]): L[A] = {
            handle2Ary(self, context)(p, _op)(new ChainPost(_, _))
        }

        override def visit[A](self: ChainPre[A], context: ParserTracker)(p: LazyParsley[A], op: =>LazyParsley[A => A]): L[A] = {
            handle2Ary(self, context)(p, op)(new ChainPre(_, _))
        }

        override def visit[A, B](self: Chainl[A, B], context: ParserTracker)
                                (init: LazyParsley[B], p: =>LazyParsley[A], op: =>LazyParsley[(B, A) => B]): L[B] = handlePossiblySeen[B](self, context) {
            zipWith3(suspend[M, R, LazyParsley[B]](init.visit(this, context)),
                     suspend[M, R, LazyParsley[A]](p.visit(this, context)),
                     suspend[M, R, LazyParsley[(B, A) => B]](op.visit(this, context)))(new Chainl[A, B](_, _, _))
        }

        override def visit[A, B](self: Chainr[A, B], context: ParserTracker)(p: LazyParsley[A], op: =>LazyParsley[(A, B) => B], wrap: A => B): L[B] = {
            handlePossiblySeen(self, context) {
                zipWith(suspend[M, R, LazyParsley[A]](p.visit(this, context)), suspend[M, R, LazyParsley[(A, B) => B]](op.visit(this, context))) {
                    new Chainr[A, B](_, _, wrap)
                }
            }
        }

        override def visit[A, C](self: SepEndBy1[A, C], context: ParserTracker)(p: LazyParsley[A], sep: =>LazyParsley[_], factory: Factory[A, C]): L[C] = {
            handlePossiblySeen(self, context) {
                zipWith(suspend[M, R, LazyParsley[A]](p.visit(this, context)), suspend[M, R, LazyParsley[_]](sep.visit(this, context))) {
                    new SepEndBy1[A, C](_, _, factory)
                }
            }
        }

        override def visit[A, C](self: ManyUntil[A, C], context: ParserTracker)(body: LazyParsley[Any], factory: Factory[A, C]): L[C] = {
            handlePossiblySeen(self, context) {
                suspend[M, R, LazyParsley[Any]](body.visit(this, context)).map(new ManyUntil[A, C](_, factory))
            }
        }

        // XXX: This will assume all completely unknown parsers have no children at all (i.e. are Singletons).
        override def visitUnknown[A](self: LazyParsley[A], context: ParserTracker): L[A] = self match {
            case d: TaggedWith[A @unchecked] => result[R, LazyParsley[A], M](d) // No need to debug a parser twice!
            case n: Named[A @unchecked]    => n.par match {
                case g: GenericLazyParsley[A @unchecked] => visitGeneric(g, context).map(_.asInstanceOf[TaggedWith[A]].withName(n.name))
                case alt: <|>[A @unchecked]              => alt.visit(this, context).map(_.asInstanceOf[TaggedWith[A]].withName(n.name))
                case cpre: ChainPre[A @unchecked]        => cpre.visit(this, context).map(_.asInstanceOf[TaggedWith[A]].withName(n.name))
                case _                                   => visitUnknown(n.par, context).map(_.asInstanceOf[TaggedWith[A]].withName(n.name))
            }
            case _                         => handleNoChildren(self, context)
        }
    }
    // $COVERAGE-ON$
}

/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.frontend.debug

import scala.collection.mutable

import org.typelevel.scalaccompat.annotation.unused

import parsley.XAssert
import parsley.debug.internal.XWeakMap
import parsley.state.Ref

import parsley.internal.deepembedding.{singletons, Cont, ContOps, Id}
import parsley.internal.deepembedding.ContOps.{perform, result, suspend, zipWith, zipWith3, ContAdapter}
import parsley.internal.deepembedding.backend.StrictParsley
import parsley.internal.deepembedding.backend.debug.TagFactory
import parsley.internal.deepembedding.frontend._ // scalastyle:ignore underscore.import

// Wrapper class signifying debugged classes
// TODO: the origin is needed to figure out the name later on... but couldn't we resolve the name here and avoid forwarding on to the backend (send string instead)?
// FIXME: this clobbers the register allocator, apparently?
private [parsley] final class TaggedWith[A](factory: TagFactory)(val origin: LazyParsley[A], val subParser: LazyParsley[A], isIterative: Boolean, userAssignedName: Option[String])
    extends LazyParsley[A] {
    XAssert.assert(!origin.isInstanceOf[TaggedWith[_]], "Tagged parsers should not be nested within each other directly.")

    def make(p: StrictParsley[A]): StrictParsley[A] = factory.create(origin, p, isIterative, userAssignedName)

    override def findLetsAux[M[_, +_] : ContOps, R](seen: Set[LazyParsley[_]])(implicit state: LetFinderState): M[R, Unit] = suspend(subParser.findLets(seen))
    override def preprocess[M[_, +_] : ContOps, R, A_ >: A](implicit lets: LetMap): M[R, StrictParsley[A_]] = {
        for (p <- suspend[M, R, StrictParsley[A]](subParser.optimised[M, R, A])) yield make(p)
    }

    // $COVERAGE-OFF$
    private [frontend] def withName(name: String): TaggedWith[A] = new TaggedWith(factory)(origin, subParser, isIterative, Some(name))
    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[A] = visitor.visitUnknown(this, context)
    // $COVERAGE-ON$

    private [parsley] var debugName = userAssignedName.getOrElse(origin.debugName)
}

private [parsley] object TaggedWith {
    // Run this to inject the debugger itself.
    def tagRecursively[A](parser: LazyParsley[A], factory: TagFactory): LazyParsley[A] = {
        // XXX: A weak map is needed so that memory leaks will not be caused by flatMap parsers.
        //      We want a decent amount of initial space to speed up debugging larger parsers slightly.
        val tracker: ParserTracker = new ParserTracker(new XWeakMap)
        if (parser.isCps) {
            implicit val ops: ContOps[Cont.Impl] = Cont.ops
            val visitor = new DebugInjectingVisitorM[Cont.Impl, LazyParsley[A]](factory)
            visitWithM[Cont.Impl, A](parser, tracker, visitor)
        } else {
            implicit val ops: ContOps[Id.Impl] = Id.ops
            val visitor = new DebugInjectingVisitorM[Id.Impl, LazyParsley[A]](factory)
            visitWithM[Id.Impl, A](parser, tracker, visitor)
        }
    }

    // This map tracks seen parsers to prevent infinitely recursive parsers from overflowing the stack (and ties
    // the knot for these recursive parsers).
    // Use maps with weak keys or don't pass this into a >>= parser.
    private final class ParserTracker(val map: mutable.Map[LazyParsley[_], ParsleyPromise[_]]) {
        def put[A](par: LazyParsley[A]): ParsleyPromise[A] = {
            val prom = new ParsleyPromise[A]
            map(par) = prom
            prom
        }
        def get[A](par: LazyParsley[A]): Deferred[LazyParsley[A]] = map(par).get.asInstanceOf[Deferred[LazyParsley[A]]]
        def hasSeen(par: LazyParsley[_]): Boolean = map.contains(par)
    }

    // these two classes are used to allow for parsers to be added into the ParserTracker map without
    // being evaluated too early: they are placed into the map before the traversal is finished (as
    // a promise), then they are retrieved lazily without forcing the value in the promise: do this
    // too early and the promise will not have been filled (this is the case for recursive parsers).
    // instead, the lazy box is opened in the by-name parameters of constructed nodes, which ensures
    // that the traversal completes before the promise is checked.
    private final class ParsleyPromise[A] {
        private var value: Deferred[LazyParsley[A]] = _
        def get: Deferred[LazyParsley[A]] = Deferred {
            if (value == null) throw new NoSuchElementException("fetched empty promise value")
            value.get
        }
        def set(v: Deferred[LazyParsley[A]]): Unit = value = v
    }
    private final class Deferred[+A](x: =>A) {
        lazy val get = x
    }
    private object Deferred {
        def apply[A](x: =>A) = new Deferred(x)
    }

    /** This class is used to store the result of a parser visit, and whether
      *  or not the parser needs to bubble up
      *
      * @param parser The parser that was visited
      * @param isIterative Whether or not the parser is transparent and hence needs to bubble up
      * @tparam A The type of the parser
      */
    private case class TaggingResult[+A](parser: Deferred[LazyParsley[A]], isIterative: Boolean)

    // Keeping this around for easy access to LPM.
    @unused private [this] final class ContWrap[M[_, +_], R] {
        type LPM[+A] = M[R, LazyParsley[A]]
         
        // Containing the related LazyParsley combinator and a Boolean for if the
        // combinator need to be bubble up to an opaque parser
         
        type DLPM[+A] = M[R, (TaggingResult[A])] 
    }

    private def visitWithM[M[_, +_]: ContOps, A](parser: LazyParsley[A], tracker: ParserTracker, visitor: DebugInjectingVisitorM[M, LazyParsley[A]]) = {
        perform[M, LazyParsley[A]](parser.visit(visitor, tracker).map(_.parser.get))
    }

    // This visitor uses Cont / ContOps to ensure that if a parser is deeply recursive, the user can all a method
    // to use the trampoline ( https://en.wikipedia.org/wiki/Trampoline_(computing) ) to ensure that all calls are
    // turned into heap thunks instead of stack frames.
    // $COVERAGE-OFF$
    private final class DebugInjectingVisitorM[M[_, +_]: ContOps, R](strategy: TagFactory)
        extends GenericLazyParsleyIVisitor[ParserTracker, ContWrap[M, R]#DLPM] {
        private type DL[+A] = ContWrap[M, R]#DLPM[A]

        private def visit[A](p: LazyParsley[A], context: ParserTracker) = suspend[M, R, TaggingResult[A]](p.visit(this, context))

        // This is the main logic for the visitor: everything else is just plumbing
        private def handlePossiblySeen[A](self: LazyParsley[A], context: ParserTracker)(subResult: =>DL[A]): DL[A] = {
            if (context.hasSeen(self)) {
                result(TaggingResult(context.get(self), self.isIterative)) 
            } else {
                val prom = context.put(self)
                subResult.map { case TaggingResult(subParser, subIsIterative) =>
                    val isIterative = self.isIterative || subIsIterative
                    // If we are opaque then attach TaggedWith now, otherwise bubble upwards
                    val retParser = {
                        if (self.isOpaque) 
                            Deferred(new TaggedWith(strategy)(self, subParser.get, isIterative, None)) 
                        else { 
                            subParser // The parser is transparent, so no tagging
                        }
                    }
                    
                    prom.set(retParser)
                    // If we are still iterative but transparent then we bubble up
                    TaggingResult(retParser, isIterative && !self.isOpaque)
                }
            }
        }

        private def handleNoChildren[A](self: LazyParsley[A], context: ParserTracker): DL[A] =
            handlePossiblySeen(self, context)(result(TaggingResult(Deferred(self), self.isIterative)))

        // We assume _q must be lazy, as it'd be better to *not* force a strict value versus accidentally forcing a lazy value.
        // This is called handle2Ary as to not be confused with handling Binary[_, _, _].
        private def handle2Ary[P[X] <: LazyParsley[X], A, B, C](self: P[A], context: ParserTracker)(p: LazyParsley[B], q: =>LazyParsley[C])
                                                               (constructor: (Deferred[LazyParsley[B]], Deferred[LazyParsley[C]]) => Deferred[P[A]]): DL[A] = {
            handlePossiblySeen[A](self, context) {
                zipWith(visit(p, context), visit(q, context))((dbgP, dbgQ) => TaggingResult(
                    parser = constructor(dbgP.parser, dbgQ.parser),
                    isIterative = dbgP.isIterative || dbgQ.isIterative
                ))
            }
        }

        override def visitSingleton[A](self: singletons.Singleton[A], context: ParserTracker): DL[A] = 
            handleNoChildren[A](self, context)

        override def visitUnary[A, B](self: Unary[A, B], context: ParserTracker)(p: LazyParsley[A]): DL[B] = handlePossiblySeen[B](self, context) {
            visit(p, context).map { dbgC => TaggingResult(
                parser = Deferred {
                    (new Unary[A, B](dbgC.parser.get) {
                        override def make(p: StrictParsley[A]): StrictParsley[B] = self.make(p)
                        override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[B] = 
                            visitor.visitGeneric(this, context)
                        private [parsley] var debugName = self.debugName
                    })},
                isIterative = dbgC.isIterative
            )}
        }

        override def visitBinary[A, B, C](self: Binary[A, B, C], context: ParserTracker)(l: LazyParsley[A], r: =>LazyParsley[B]): DL[C] = {
            handlePossiblySeen(self, context) {
                zipWith(visit(l, context), visit(r, context)) { (dbgL, dbgR) => TaggingResult(
                    parser = Deferred {
                        new Binary[A, B, C](dbgL.parser.get, dbgR.parser.get) {
                            override def make(p: StrictParsley[A], q: StrictParsley[B]): StrictParsley[C] = self.make(p, q)
                            override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[C] = 
                                visitor.visitGeneric(this, context)
                            private [parsley] var debugName = self.debugName
                        }
                    },
                    isIterative = dbgL.isIterative || dbgR.isIterative
                )}
            }
        }

        override def visitTernary[A, B, C, D](self: Ternary[A, B, C, D], context: ParserTracker)
                                             (f: LazyParsley[A], s: =>LazyParsley[B], t: =>LazyParsley[C]): DL[D] = handlePossiblySeen[D](self, context) {
            zipWith3(visit(f, context), visit(s, context), visit(t, context)) { (dbgF, dbgS, dbgT) => TaggingResult(
                parser = Deferred {
                    new Ternary[A, B, C, D](dbgF.parser.get, dbgS.parser.get, dbgT.parser.get) {
                        override def make(p: StrictParsley[A], q: StrictParsley[B], r: StrictParsley[C]): StrictParsley[D] = self.make(p, q, r)
                        override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[D] = visitor.visitGeneric(this, context)
                        private [parsley] var debugName = self.debugName
                    }
                },
                isIterative = dbgF.isIterative || dbgS.isIterative || dbgT.isIterative
            )}
        }

        // We want flatMap-produced parsers to be debugged too, so we can see the full extent of the produced parse tree.
        // This is critical, as flatMap allows these parsers to be turing-complete, and can produce any arbitrary parse path.
        override def visit[A, B](self: A >>= B, context: ParserTracker)(p: LazyParsley[A], f: A => LazyParsley[B]): DL[B] = {
            handlePossiblySeen(self, context) {
                // flatMap / >>= produces parsers arbitrarily, so there is no way we'd match by reference.
                // This is why a map with weak keys is required, so that these entries do not flood the map and
                // cause a massive memory leak.
                visit(p, context).map { dbgC => TaggingResult(
                    parser = Deferred {
                        new >>=[A, B](dbgC.parser.get, x => {
                            val subVisitor = new DebugInjectingVisitorM[M, LazyParsley[B]](strategy)
                            perform[M, LazyParsley[B]](f(x).visit(subVisitor, context).map(_.parser.get))
                        })
                    },
                    isIterative = dbgC.isIterative
                )}
            }
        }

        override def visit[A](self: <|>[A], context: ParserTracker)(p: LazyParsley[A], q: LazyParsley[A]): DL[A] = {
            handle2Ary(self, context)(p, q) { (p, q) => {
                    Deferred(new <|>(p.get, q.get, self.debugName))
                }
            }
        }

        override def visit[A](self: ChainPre[A], context: ParserTracker)(p: LazyParsley[A], op: =>LazyParsley[A => A]): DL[A] = {
            handle2Ary(self, context)(p, op) { (p, op) => {
                    Deferred(new ChainPre(p.get, op.get))
                }
            }
        } 

        // the generic unary/binary overrides above cannot handle this properly, as they lose the UsesReg trait
        override def visit[S](self: Put[S], context: ParserTracker)(ref: Ref[S], p: LazyParsley[S]): DL[Unit] = {
            handlePossiblySeen(self, context) {
                visit(p, context).map(p => { TaggingResult(
                    parser = Deferred( new Put(ref, p.parser.get)),
                    isIterative = p.isIterative
                )})
            }
        }
        override def visit[S, A](self: NewReg[S, A], context: ParserTracker)(ref: Ref[S], init: LazyParsley[S], body: =>LazyParsley[A]): DL[A] = {
            handlePossiblySeen(self, context) {
                zipWith(visit(init, context), visit(body, context)) { (init, body) => TaggingResult(
                    parser = Deferred {
                        new NewReg(ref, init.parser.get, body.parser.get)
                    },
                    isIterative = init.isIterative || body.isIterative
                )}
            }
        }

        // XXX: This will assume all completely unknown parsers have no children at all (i.e. are Singletons).
        override def visitUnknown[A](self: LazyParsley[A], context: ParserTracker): DL[A] = self match {
            case d: TaggedWith[A @unchecked]      => 
                result[R, TaggingResult[A], M](TaggingResult(new Deferred(d), d.origin.isIterative)) // No need to debug a parser twice!
            case n: Named[A @unchecked]           => n.p.visit(this, context).map { subResult =>
                subResult.parser.get match {
                    case tw: TaggedWith[A @unchecked] => TaggingResult(new Deferred(tw.withName(n.name)), subResult.isIterative)
                    // this should never be the case, because all all opaque combinators will be tagged,
                    // so if it's not tagged it will be transparent, and naming a transparent combinator
                    // goes against it being transparent. Additionally, named is not available within
                    // parsley, and parsley does not expose naturally transparent combinators.
                    case _                            => throw new IllegalStateException("a transparent parser has been explicitly named, this is non-sensical")
                }
            }
            case rb: RemoteBreak[A @unchecked]    => visitUnary(rb, context)(rb.p)
            case _                                => handleNoChildren(self, context)
        }
    }
    // $COVERAGE-ON$
}

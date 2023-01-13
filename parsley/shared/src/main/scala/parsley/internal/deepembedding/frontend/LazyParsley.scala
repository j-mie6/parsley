/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.frontend

import scala.annotation.nowarn
import scala.collection.mutable

import parsley.XAssert._
import parsley.exceptions.BadLazinessException
import parsley.registers.Reg

import parsley.internal.deepembedding.{Cont, ContOps, Id}, ContOps.{perform, result, ContAdapter}
import parsley.internal.deepembedding.backend, backend.StrictParsley
import parsley.internal.machine.instructions, instructions.Instr

/** This is the root type of the parsley "frontend": it represents a combinator tree
  * where the join-points in the tree (recursive or otherwise) have not been identified
  * or factored. As such, it is a potentially cyclic graph (though finite), and must be handled with
  * caution.
  *
  * @note objects of this type may be shared across parsers or threads and, as such,
  *       must remain entirely immutable.
  */
private [parsley] abstract class LazyParsley[+A] private [deepembedding] {
    // Public API
    // $COVERAGE-OFF$
    /** Denotes this parser is unsafe, which will disable certain law-based optimisations that assume purity. */
    private [parsley] final def unsafe(): Unit = sSafe = false
    /** Force the parser, which eagerly computes its instructions immediately */
    private [parsley] final def force(): Unit = instrs: @nowarn
    /** Denote that this parser is large enough that it might stack-overflow during
      * compilation: this allows for the slow path using `Cont` to be used immediately
      * instead of going through the (likely failing) `Id` path.
      */
    private [parsley] final def overflows(): Unit = cps = true
    // $COVERAGE-ON$

    // The instructions used to execute this parser along with the number of registers it uses
    final private [parsley] lazy val (instrs: Array[Instr], numRegs: Int) = computeInstrs

    /** This parser is the result of a `flatMap` operation, and as such must perform
      * callee-save on `numRegs` registers (which belong to its parent)
      *
      * @param numRegs the number of registers the parent uses (these must be saved)
      */
    private [deepembedding] def demandCalleeSave(numRegs: Int): this.type = {
        numRegsUsedByParent = numRegs
        this
    }

    // Internals
    // To ensure that stack-overflow cannot occur during the processing of particularly
    // large parsers, the entire internals of the "frontend" and "backend" of parsley is
    // performed via the Monad of Continuations, or `Cont`. This allows for the execution
    // of the methods below to be trampolined, which evaluates them in a loop, trading
    // stack-space for heap-space. Each method is parameterised, however, by an ''abstract''
    // `Cont`, because in the event that a parser doesn't stack overflow under normal
    // execution, it is preferable to evaluate it under the much lighter-weight Identity
    // Monad, or `Id`. The choice of monad is delegated to `computeInstrs`.
    //
    // The frontend is split into two passes: the first identifies all the shared parsers
    // within the combinator tree; and the second factors these parsers out and converts
    // the combinator tree into its strict, finite, form: `StrictParsley`.
    //
    // Pass 1: `findLets` (using `findLetsAux`) populates a `LetFinderState`
    // Pass 2: extracts the `lets` and `recs` from the state, and feeds through `optimised`
    //         (using `preprocess`)

    /** Describes how to recursively traverse this combinators sub-trees using `findLets`.
      *
      * @param seen the set of all nodes that have previously been seen by the let-finding
      * @param state stores all the information of the let-finding process
      */
    protected def findLetsAux[Cont[_, +_]: ContOps, R](seen: Set[LazyParsley[_]])(implicit state: LetFinderState): Cont[R, Unit]

    /** Describes how to recursively convert this combinator into a `StrictParsley` by
      * `optimise`ing its sub-trees.
      *
      * @param lets the known non-recursive shared parsers mapped to their corresponding join-point nodes
      * @param recs the known recursive parsers mapped to their corresponding join-point nodes
      * @return the strict, finite, version of this tree, with all shared parsers factored out into join-points
      */
    protected def preprocess[Cont[_, +_]: ContOps, R, A_ >: A](implicit lets: LetMap, recs: RecMap): Cont[R, StrictParsley[A_]]

    /** should the underlying strict tree be considered safe? */
    final private var sSafe = true
    /** should the `Id` instance be skipped? */
    final private var cps = false
    /** how many registers are used by the ''parent'' of this combinator (this combinator is part of a `flatMap` when this is not -1) */
    final private var numRegsUsedByParent = -1

    /** Computes the instructions associated with this parser as well as the number of
      * registers it requires in a (possibly) stack-safe way.
      */
    final private def computeInstrs: (Array[Instr], Int) = {
        if (cps) computeInstrs(Cont.ops) else computeInstrs(Id.ops)
    }
    /** Computes the instructions associated with this parser as well as the number of
      * registers it requires within the context of a specific (unknown) monad.
      *
      * @param ops the instance for the monad to evaluate with
      */
    final private def computeInstrs[Cont[_, +_]](ops: ContOps[Cont]): (Array[Instr], Int) = pipeline(ops)

    /** Performs the full end-to-end pipeline through both the frontend and the backend.
      *
      * First performs let-finding to identify the shared parsers (recursive or otherwise)
      * within the combinator tree. Then performs let-factoring and (optimising) conversion
      * to `StrictParsley`. Then executes the backend pipeline on this strict combinator tree,
      * yielding the final results.
      *
      * @return the instructions associates with this parser as well as the number of
      *         registers it requires
      */
    final private def pipeline[Cont[_, +_]: ContOps]: (Array[Instr], Int) = {
        implicit val letFinderState: LetFinderState = new LetFinderState
        (perform[Cont, Array[Instr]] {
            findLets(Set.empty) >> {
                val usedRegs: Set[Reg[_]] = letFinderState.usedRegs
                implicit val state: backend.CodeGenState = new backend.CodeGenState(letFinderState.numRegs)
                implicit val recMap: RecMap = RecMap(letFinderState.recs)
                implicit val letMap: LetMap = LetMap(letFinderState.lets)
                val recs_ = recMap.map { case (p, rec) => (rec, p.unsafeOptimised[Cont, Unit, Any]) }
                for { sp <- this.optimised } yield sp.generateInstructions(numRegsUsedByParent, usedRegs, recs_)
            }
        }, letFinderState.numRegs)
    }

    // Pass 1
    /** A recursive process that identifies shared parsers within the combinator tree.
      *
      * - first increments the "predecessor" count of this parser: this is the number of
      *   other parsers (or the top-level) that reference. When this count is more than 1
      *   this parser will be considered as shared
      * - checks whether this parser has been encountered before during the analysis: if
      *   so it is recursive, so add it to the recs set: this node '''must''' not be processed,
      *   or it will cause an infinite divergence.
      * - otherwise add any used registers to the register set and recurse
      *
      * @param seen the set of all nodes that have previously been seen by the let-finding
      * @param state stores all the information of the let-finding process
      */
    @throws[BadLazinessException]("if this parser references another parser before it has been initialised")
    final protected [frontend] def findLets[Cont[_, +_]: ContOps, R](seen: Set[LazyParsley[_]])(implicit state: LetFinderState): Cont[R, Unit] = {
        state.addPred(this)
        if (seen.contains(this)) result(state.addRec(this))
        else if (state.notProcessedBefore(this)) {
            this match {
                case self: UsesRegister => state.addReg(self.reg)
                case _                  =>
            }

            try findLetsAux(seen + this)
            catch {
                // $COVERAGE-OFF$
                case _: NullPointerException => throw new BadLazinessException // scalastyle:ignore throw
                // $COVERAGE-ON$
            }
        }
        else result(())
    }

    // Pass 2
    /** Performs the factoring out of shared parsers and then converts this parser into its strict form performing
      * optimisations on that translated form.
      *
      * @param lets the known non-recursive shared parsers mapped to their corresponding join-point nodes
      * @param recs the known recursive parsers mapped to their corresponding join-point nodes
      * @return the strict, finite, version of this tree, with all shared parsers factored out into join-points
      */
    final protected [frontend] def optimised[Cont[_, +_]: ContOps, R, A_ >: A](implicit lets: LetMap, recs: RecMap): Cont[R, StrictParsley[A_]] = {
        if (recs.contains(this)) result(recs(this))
        else if (lets.contains(this)) result(lets(this))
        else this.unsafeOptimised
    }
    /** Similar to `optimised` but should be '''only''' used on things known to be let-bindings (to avoid infinite expansion!). */
    final private [frontend] def knownLetTopOptimised[Cont[_, +_]: ContOps, R, A_ >: A](implicit lets: LetMap, recs: RecMap): Cont[R, StrictParsley[A_]] = {
        assume(lets.contains(this), "the let check can only be skipped for known let-bindings")
        assume(!recs.contains(this), "rec membership can be skipped for known let-binding bodies")
        this.unsafeOptimised
    }
    /** Similar to `optimised` but does not check for inclusion in the `lets` or `recs` sets. */
    private def unsafeOptimised[Cont[_, +_]: ContOps, R, A_ >: A](implicit lets: LetMap, recs: RecMap): Cont[R, StrictParsley[A_]] = {
        for {p <- this.preprocess} yield {
            p.safe = this.sSafe
            p.optimise
        }
    }

    // $COVERAGE-OFF$
    /** Pretty-prints a combinator tree, for internal debugging purposes only. */
    final private [internal] def prettyAST: String = {
        import Cont.ops // scalastyle:ignore import.grouping
        implicit val letFinderState: LetFinderState = new LetFinderState
        perform[Cont, String] {
            findLets(Set.empty) >> {
                implicit val state: backend.CodeGenState = new backend.CodeGenState(0)
                implicit val recMap: RecMap = RecMap(letFinderState.recs)
                implicit val letMap: LetMap = LetMap(letFinderState.lets)
                val mrecs = for {
                    (p, rec) <- recMap
                } yield for {
                    sp <- p.unsafeOptimised[Cont, String, Any]
                    str <- sp.pretty
                } yield s"${rec.label}: $str"

                for {
                    sp <- this.optimised
                    str <- sp.pretty
                    strs <- ContOps.sequence(mrecs.toList)
                } yield {
                    s"main body: $str\n${strs.mkString("\n")}"
                }
            }
        }
    }
    // $COVERAGE-ON$
}

/** A mix-in trait that denotes that this parser uses a specific register, which must be allocated. */
private [deepembedding] trait UsesRegister {
    /** The register used by this combinator. */
    val reg: Reg[_]
}

/** This is a collection of builders that track the shared parsers and used registers during Pass 1 */
private [deepembedding] class LetFinderState {
    private val _recs = mutable.Set.empty[LazyParsley[_]]
    private val _preds = mutable.Map.empty[LazyParsley[_], Int]
    private val _usedRegs = mutable.Set.empty[Reg[_]]

    /** Adds a "predecessor" to a given parser, which means that it is referenced by another parser.
      *
      * @note assumes that a parser isn't given a predecessor by the same root twice.
      *
      * @param p the parser to add a predecessor to
      */
    private [frontend] def addPred(p: LazyParsley[_]): Unit = _preds(p) = _preds.getOrElse(p, 0) + 1
    /** If a parser is identified as being recursive, keep track of it.
      *
      * @param p a recursive parser
      */
    private [frontend] def addRec(p: LazyParsley[_]): Unit = _recs += p
    /** If a register has been used by a parser, keep track of it.
      *
      * @param reg the register used by the parser.
      */
    private [frontend] def addReg(reg: Reg[_]): Unit = _usedRegs += reg
    /** Has the given parser never been analysed before? */
    private [frontend] def notProcessedBefore(p: LazyParsley[_]): Boolean = _preds(p) == 1

    /** Returns all the non-recursive parsers which are referenced two or more times across the tree. */
    private [frontend] def lets: Iterable[LazyParsley[_]] = _preds.toSeq.view.collect {
        case (p, refs) if refs >= 2 && !_recs(p) => p
    }
    /** Returns all the recursive parsers in the tree */
    private [frontend] lazy val recs: Set[LazyParsley[_]] = _recs.toSet
    /** Returns all the registers used by the parser */
    private [frontend] def usedRegs: Set[Reg[_]] = _usedRegs.toSet
    /** Returns the number of registers used by the parser */
    private [frontend] def numRegs: Int = _usedRegs.size
}

/** Represents a map of let-bound lazy parsers to their strict equivalents. */
private [deepembedding] final class LetMap private (letGen: Map[LazyParsley[_], LetMap => StrictParsley[_]]) {
    // This might not necessarily contain Let nodes: if they were inlined then they will not be present here
    private val mutMap = mutable.Map.empty[LazyParsley[_], StrictParsley[_]]

    /** Is the given parser a let-binding? */
    def contains(p: LazyParsley[_]): Boolean = letGen.contains(p)

    /** Returns the strict parser that represents a given let-bound parser.
      *
      * @note this does not necessary return a `Let` node, as the underlying parser may be inlined.
      */
    def apply[A](p: LazyParsley[A]): StrictParsley[A] = mutMap.getOrElseUpdate(p, {
        assume(contains(p), "only let-bound parsers can be mapped to a strict version in the let-map")
        val sp = letGen(p)(this)
        if (sp.inlinable) sp else new backend.Let(sp)
    }).asInstanceOf[StrictParsley[A]]

    // $COVERAGE-OFF$
    override def toString: String = mutMap.toString
    // $COVERAGE-ON$
}
private [frontend] object LetMap {
    /** Builds a `LetMap` given the sequence of let-bound parsers as well as the map of recursive parsers
      *
      * @param lets the identified shared non-recursive parsers to include
      * @param recs the identified recursive parsers that may be required in the translation
      */
    def apply[Cont[_, +_]: ContOps](lets: Iterable[LazyParsley[_]])(implicit recs: RecMap): LetMap = {
        new LetMap(lets.map(p => p -> ((_self: LetMap) => {
            implicit val self: LetMap = _self
            perform[Cont, StrictParsley[_]](p.knownLetTopOptimised)
        })).toMap)
    }
}

/** Represents the map of lazy recursive parsers to their strict `Rec` node join-points. */
private [deepembedding] final class RecMap private (map: Map[LazyParsley[_], backend.Rec[_]]) extends Iterable[(LazyParsley[_], backend.Rec[_])] {
    /** Is the given parser recursive? */
    def contains(p: LazyParsley[_]): Boolean = map.contains(p)

    /** Returns the `Rec` node that corresponds to a given recursive parser. */
    def apply[A](p: LazyParsley[A]): backend.Rec[A] = map(p).asInstanceOf[backend.Rec[A]]

    /** An iterator over all the key-value pairs in this map */
    override def iterator: Iterator[(LazyParsley[_], backend.Rec[_])] = map.iterator

    // $COVERAGE-OFF$
    override def toString: String = map.toString
    // $COVERAGE-ON$
}
private [frontend] object RecMap {
    /** Creates a `RecMap` given all the recursive parsers identified by let-finding.
      * This will map each parser to a `Rec` node in the strict combinator tree.
      *
      * @param recs all of the recursive parsers to fill the map with
      * @param state code-generation state, required to generate labels for the `Call` instructions.
      */
    def apply(recs: Iterable[LazyParsley[_]])(implicit state: backend.CodeGenState): RecMap = {
        new RecMap(recs.map(p => p -> new backend.Rec(new instructions.Call(state.freshLabel()))).toMap)
    }
}

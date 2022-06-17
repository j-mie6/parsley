/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.frontend

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.{higherKinds, implicitConversions}

import parsley.BadLazinessException
import parsley.registers.Reg

import parsley.internal.deepembedding.{Cont, ContOps}, ContOps.{safeCall, GenOps, perform, result, ContAdapter}
import parsley.internal.deepembedding.backend, backend.StrictParsley
import parsley.internal.machine.instructions, instructions.{Instr, JumpTable, Label}

private [parsley] abstract class LazyParsley[+A] private [deepembedding] {
    // $COVERAGE-OFF$
    final def unsafe(): Unit = sSafe = false
    final def force(): Unit = instrs
    final def overflows(): Unit = cps = true
    // $COVERAGE-ON$
    private [deepembedding] def demandCalleeSave(): this.type = {
        calleeSaveNeeded = true
        this
    }

    // Internals
    final private [frontend] def findLets[Cont[_, +_]: ContOps, R](seen: Set[LazyParsley[_]])(implicit state: LetFinderState): Cont[R, Unit] = {
        state.addPred(this)
        if (seen(this)) result(state.addRec(this))
        else if (state.notProcessedBefore(this)) {
            this match {
                case self: UsesRegister => state.addReg(self.reg)
                case _                  =>
            }

            try findLetsAux(seen + this)
            catch {
                // $COVERAGE-OFF$
                case npe: NullPointerException => throw new BadLazinessException // scalastyle:ignore throw
                // $COVERAGE-ON$
            }
        }
        else result(())
    }
    final private [frontend] def optimised[Cont[_, +_]: ContOps, R, A_ >: A](implicit lets: LetMap, recs: RecMap): Cont[R, StrictParsley[A_]] = {
        if (recs.contains(this)) result(recs(this))
        else if (lets.contains(this)) result(lets(this))
        else this.unsafeOptimised
    }
    final private [frontend] def unsafeOptimised[Cont[_, +_]: ContOps, R, A_ >: A](implicit lets: LetMap, recs: RecMap): Cont[R, StrictParsley[A_]] = {
        for {p <- this.preprocess} yield {
            p.safe = this.sSafe
            p.optimise
        }
    }
    final private [deepembedding] var sSafe = true
    final private var cps = false
    final private var calleeSaveNeeded = false

    final private def pipeline[Cont[_, +_]: ContOps]: Array[Instr] = {
        implicit val letFinderState: LetFinderState = new LetFinderState
        perform[Cont, Array[Instr]] {
            findLets(Set.empty) >> {
                val usedRegs: Set[Reg[_]] = letFinderState.usedRegs
                implicit val seenSet: Set[LazyParsley[_]] = letFinderState.recs
                implicit val state: backend.CodeGenState = new backend.CodeGenState
                implicit val recMap: RecMap = RecMap(letFinderState.recs)
                implicit val letMap: LetMap = LetMap(letFinderState.lets)
                val recs_ = recMap.map { case (p, rec) => (rec, p.unsafeOptimised[Cont, Unit, Any]) }
                for { sp <- this.optimised } yield sp.generateInstructions(calleeSaveNeeded, usedRegs, recs_)
            }
        }
    }

    final private def computeInstrs(ops: GenOps): Array[Instr] = pipeline(ops)

    final private [parsley] lazy val instrs: Array[Instr] = if (cps) computeInstrs(Cont.ops.asInstanceOf[GenOps]) else safeCall(computeInstrs(_))

    // $COVERAGE-OFF$
    final private [internal] def prettyAST: String = {
        import Cont.ops // scalastyle:ignore import.grouping
        implicit val letFinderState: LetFinderState = new LetFinderState
        perform[Cont, String] {
            findLets(Set.empty) >> {
                val usedRegs: Set[Reg[_]] = letFinderState.usedRegs
                implicit val seenSet: Set[LazyParsley[_]] = letFinderState.recs
                implicit val state: backend.CodeGenState = new backend.CodeGenState
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

    // Abstracts
    // Sub-tree optimisation and Rec calculation - Bottom-up
    private [frontend] def preprocess[Cont[_, +_]: ContOps, R, A_ >: A](implicit lets: LetMap, recs: RecMap): Cont[R, StrictParsley[A_]]
    // Let-finder recursion
    protected def findLetsAux[Cont[_, +_]: ContOps, R](seen: Set[LazyParsley[_]])(implicit state: LetFinderState): Cont[R, Unit]
}

private [deepembedding] trait UsesRegister {
    val reg: Reg[_]
}

private [deepembedding] class LetFinderState {
    private val _recs = mutable.Set.empty[LazyParsley[_]]
    private val _preds = mutable.Map.empty[LazyParsley[_], Int]
    private val _usedRegs = mutable.Set.empty[Reg[_]]

    def addPred(p: LazyParsley[_]): Unit = _preds += p -> (_preds.getOrElseUpdate(p, 0) + 1)
    def addRec(p: LazyParsley[_]): Unit = _recs += p
    def addReg(reg: Reg[_]): Unit = _usedRegs += reg
    def notProcessedBefore(p: LazyParsley[_]): Boolean = _preds(p) == 1

    def lets: Iterable[LazyParsley[_]] = _preds.toSeq.view.collect {
        case (p, refs) if refs >= 2 && !_recs(p) => p
    }
    lazy val recs: Set[LazyParsley[_]] = _recs.toSet
    def usedRegs: Set[Reg[_]] = _usedRegs.toSet
}

private [deepembedding] final class LetMap(letGen: Map[LazyParsley[_], LetMap => StrictParsley[_]]) {
    // This might not necessarily contain Let nodes: if they were inlined then they will not be present here
    private val mutMap = mutable.Map.empty[LazyParsley[_], StrictParsley[_]]

    def contains(p: LazyParsley[_]): Boolean = letGen.contains(p)
    def apply[A](p: LazyParsley[A]): StrictParsley[A] = mutMap.getOrElseUpdate(p, {
        val sp = letGen(p)(this)
        if (sp.inlinable) sp else new backend.Let(sp)
    }).asInstanceOf[StrictParsley[A]]
    override def toString: String = mutMap.toString
}
private [frontend] object LetMap {
    def apply[Cont[_, +_]: ContOps](lets: Iterable[LazyParsley[_]])(implicit recs: RecMap): LetMap = {
        new LetMap(lets.map(p => p -> ((_self: LetMap) => {
            implicit val self: LetMap = _self
            perform[Cont, StrictParsley[_]](p.unsafeOptimised)
        })).toMap)
    }
}

private [deepembedding] final class RecMap(map: Map[LazyParsley[_], backend.Rec[_]]) extends Iterable[(LazyParsley[_], backend.Rec[_])] {
    def contains(p: LazyParsley[_]): Boolean = map.contains(p)
    def apply[A](p: LazyParsley[A]): backend.Rec[A] = map(p).asInstanceOf[backend.Rec[A]]
    override def toString: String = map.toString
    override def iterator: Iterator[(LazyParsley[_], backend.Rec[_])] = map.iterator
}
private [frontend] object RecMap {
    def apply(recs: Iterable[LazyParsley[_]])(implicit state: backend.CodeGenState): RecMap = {
        new RecMap(recs.map(p => p -> new backend.Rec(new instructions.Call(state.freshLabel()))).toMap)
    }
}

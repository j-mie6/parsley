/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.backend

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.{higherKinds, implicitConversions}

import parsley.BadLazinessException
import parsley.registers.Reg

import parsley.internal.collection.mutable.ResizableArray
import parsley.internal.deepembedding.ContOps, ContOps.{safeCall, GenOps, perform, result, ContAdapter}
import parsley.internal.machine.instructions, instructions.{Instr, JumpTable, Label}

import StrictParsley._

private [deepembedding] trait StrictParsley[+A] {
    final protected type T = Any
    final protected type U = Any

    private [deepembedding] def inlinable: Boolean
    final private [deepembedding] var safe = true

    final private [deepembedding] def generateInstructions[Cont[_, +_]: ContOps](numRegsUsedByParent: Int, usedRegs: Set[Reg[_]],
                                                                                 recs: Iterable[(Rec[_], Cont[Unit, StrictParsley[_]])])
                                                                                (implicit state: CodeGenState): Array[Instr] = {
        implicit val instrs: InstrBuffer = new ResizableArray()
        val bindings = mutable.ListBuffer.empty[Binding]
        perform {
            generateCalleeSave[Cont, Array[Instr]](numRegsUsedByParent, this.codeGen, usedRegs.size, allocateRegisters(usedRegs)) |> {
                instrs += instructions.Halt
                finaliseRecs(recs)
                finaliseLets(bindings)
                generateHandlers(state.handlers)
                finaliseInstrs(instrs, state, recs.map(_._1), bindings.toList)
            }
        }
    }

    final private def generateHandlers(handlers: Iterator[(Instr, Int)])(implicit instrs: InstrBuffer): Unit = {
        for ((handler, label) <- handlers) {
            instrs += new instructions.Label(label)
            instrs += handler
        }
    }

    // This is a trick to get tail-calls to fire even in the presence of a legimate recursion
    @inline final protected def optimiseDefinitelyNotTailRec: StrictParsley[A] = optimise
    protected  [deepembedding] def optimise: StrictParsley[A] = this

    // Peephole optimisation and code generation - Top-down
    private [backend] def codeGen[Cont[_, +_]: ContOps, R](implicit instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit]

    // $COVERAGE-OFF$
    private [deepembedding] def pretty[Cont[_, +_]: ContOps, R]: Cont[R, String]
    // $COVERAGE-ON$
}

private [deepembedding] object StrictParsley {
    private [deepembedding] type InstrBuffer = ResizableArray[Instr]

    private def applyAllocation(regs: Set[Reg[_]], freeSlots: Iterable[Int]): List[Int] = {
        val allocatedSlots = mutable.ListBuffer.empty[Int]
        for ((reg, addr) <- regs.zip(freeSlots)) {
            reg.allocate(addr)
            allocatedSlots += addr
        }
        allocatedSlots.toList
    }

    private def allocateRegisters(regs: Set[Reg[_]]): List[Int] = {
        // Global registers cannot occupy the same slot as another global register
        // In a flatMap, that means a newly discovered global register must be allocated to a new slot: this may resize the register pool
        val unallocatedRegs = regs.filterNot(_.allocated)
        if (unallocatedRegs.nonEmpty) {
            val usedSlots = regs.collect {
                case reg if reg.allocated => reg.addr
            }
            val freeSlots = (0 until regs.size).filterNot(usedSlots)
            applyAllocation(unallocatedRegs, freeSlots)
        }
        else Nil
    }

    private def generateCalleeSave[Cont[_, +_]: ContOps, R](numRegsUsedByParent: Int, bodyGen: =>Cont[R, Unit], reqRegs: Int, allocatedRegs: List[Int])
                                                           (implicit instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        val calleeSaveRequired = numRegsUsedByParent >= 0 // if this is -1, then we are the top level and have no parent, otherwise it needs to be done
        if (calleeSaveRequired && allocatedRegs.nonEmpty) {
            val end = state.freshLabel()
            val calleeSave = state.freshLabel()
            instrs += new instructions.Label(calleeSave)
            instrs += new instructions.CalleeSave(end, reqRegs, allocatedRegs, numRegsUsedByParent)
            bodyGen |> {
                instrs += new instructions.Jump(calleeSave)
                instrs += new instructions.Label(end)
            }
        }
        else bodyGen
    }

    private def finaliseRecs[Cont[_, +_]: ContOps](recs: Iterable[(Rec[_], Cont[Unit, StrictParsley[_]])])
                                                  (implicit instrs: InstrBuffer, state: CodeGenState): Unit = {
        for ((rec, p) <- recs) {
            instrs += new instructions.Label(rec.label)
            perform(p.flatMap(_.codeGen))
            instrs += instructions.Return
        }
    }

    private def finaliseLets[Cont[_, +_]: ContOps](bindings: mutable.ListBuffer[Binding])
                                                  (implicit instrs: InstrBuffer, state: CodeGenState): Unit = {
        while (state.more) {
            val let = state.nextLet()
            bindings += let
            instrs += new instructions.Label(let.label)
            perform[Cont, Unit](let.p.codeGen)
            instrs += instructions.Return
        }
    }

    // Applies tail-call optimisation
    private def tco(instrs: Array[Instr], labels: Array[Int], bindings: List[Binding])(implicit state: CodeGenState): Unit = if (bindings.nonEmpty) {
        val bindingsWithReturns = bindings.zip(bindings.tail.map(_.location(labels) - 1) :+ (instrs.size-1))
        lazy val locToBinding = bindings.map(b => b.location(labels) -> b).toMap
        for ((binding, retLoc) <- bindingsWithReturns) instrs(retLoc-1) match {
            case instr: instructions.Call => instrs(retLoc-1) = new instructions.Jump(instr.label)
            case _ =>
        }
    }

    private def finaliseInstrs(instrs: InstrBuffer, state: CodeGenState, recs: Iterable[Rec[_]], bindings: List[Binding]): Array[Instr] = {
        @tailrec def findLabels(instrs: Array[Instr], labels: Array[Int], n: Int, i: Int, off: Int): Int = if (i + off < n) instrs(i + off) match {
            case label: Label =>
                instrs(i + off) = null
                labels(label.i) = i
                findLabels(instrs, labels, n, i, off + 1)
            case _ => findLabels(instrs, labels, n, i + 1, off)
        }
        else i
        @tailrec def applyLabels(srcs: Array[Instr], labels: Array[Int], dests: Array[Instr], n: Int, i: Int, off: Int): Unit = {
            if (i < n) srcs(i + off) match {
                case null => applyLabels(srcs, labels, dests, n, i, off + 1)
                case instr =>
                    dests(i) = instr.relabel(labels)
                    applyLabels(srcs, labels, dests, n, i + 1, off)
            }
        }
        val instrsOversize = instrs.toArray
        val labelMapping = new Array[Int](state.nlabels)
        val size = findLabels(instrsOversize, labelMapping, instrs.length, 0, 0)
        val instrs_ = new Array[Instr](size)
        applyLabels(instrsOversize, labelMapping, instrs_, instrs_.length, 0, 0)
        tco(instrs_, labelMapping, bindings)(state)
        instrs_
    }
}

private [backend] trait Binding { self: StrictParsley[_] =>
    // When these are used by tco, the call instructions labels have already been shifted, but lets have not
    final def location(labelMap: Array[Int])(implicit state: CodeGenState): Int = this match {
        case self: Rec[_] => self.label
        case self: Let[_] => labelMap(self.label)
    }

    // $COVERAGE-OFF$
    def pretty[Cont[_, +_]: ContOps, R]: Cont[R, String] = result(this.toString())
    // $COVERAGE-ON$
}
private [deepembedding] trait MZero extends StrictParsley[Nothing]

// Internals
private [deepembedding] class CodeGenState(val numRegs: Int) {
    private var current = 0
    private val queue = mutable.ListBuffer.empty[Let[_]]
    private val map = mutable.Map.empty[Let[_], Int]
    def freshLabel(): Int = {
        val next = current
        current += 1
        next
    }
    def nlabels: Int = current

    def getLabel(sub: Let[_]): Int = map.getOrElseUpdate(sub, {
        sub +=: queue
        freshLabel()
    })

    def nextLet(): Let[_] = queue.remove(0)
    def more: Boolean = queue.nonEmpty
    def subsExist: Boolean = map.nonEmpty

    private val handlerMap = mutable.Map.empty[Instr, Int]
    private val relabelErrorMap = mutable.Map.empty[String, Int]
    private val applyReasonMap = mutable.Map.empty[String, Int]
    private val putAndFailMap = mutable.Map.empty[Reg[_], Int]
    def getLabel(handler: Instr): Int  = handlerMap.getOrElseUpdate(handler, freshLabel())
    def getLabelForRelabelError(label: String): Int = relabelErrorMap.getOrElseUpdate(label, freshLabel())
    def getLabelForApplyReason(reason: String): Int = applyReasonMap.getOrElseUpdate(reason, freshLabel())
    def getLabelForPutAndFail(reg: Reg[_]): Int = putAndFailMap.getOrElseUpdate(reg, freshLabel())
    def handlers: Iterator[(Instr, Int)] = {
        val relabelErrors = relabelErrorMap.view.map {
            case (label, i) => new instructions.RelabelErrorAndFail(label) -> i
        }
        val applyReasons = applyReasonMap.view.map {
            case (reason, i) => new instructions.ApplyReasonAndFail(reason) -> i
        }
        val putAndFail = putAndFailMap.view.map {
            case (reg, i) => new instructions.PutAndFail(reg.addr) -> i
        }
        new Iterator[(Instr, Int)] {
            private var rest = List(relabelErrors.iterator, applyReasons.iterator, putAndFail.iterator)
            private var cur = handlerMap.iterator
            override def hasNext: Boolean = {
                cur.hasNext || (rest.nonEmpty && {
                    cur = rest.head
                    rest = rest.tail
                    this.hasNext
                })
            }

            override def next(): (Instr, Int) = cur.next()
        }
    }
}

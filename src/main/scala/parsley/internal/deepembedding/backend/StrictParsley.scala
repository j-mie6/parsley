/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.backend

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.{higherKinds, implicitConversions}

import parsley.BadLazinessException
import parsley.registers.Reg

import parsley.internal.deepembedding.ContOps, ContOps.{safeCall, GenOps, perform, result, ContAdapter}
import parsley.internal.machine.instructions, instructions.{Instr, JumpTable, Label}

import StrictParsley._

private [deepembedding] trait StrictParsley[+A] {
    final protected type T = Any
    final protected type U = Any

    private [deepembedding] def inlinable: Boolean
    final private [deepembedding] var safe = true

    final private [deepembedding] def generateInstructions[Cont[_, +_]](calleeSaveRequired: Boolean, usedRegs: Set[Reg[_]],
                                                                        recs: Iterable[(Rec[_], Cont[Unit, StrictParsley[_]])])
                                                                       (implicit ops: ContOps[Cont], state: CodeGenState): Array[Instr] ={
        implicit val instrs: InstrBuffer = new ResizableArray()
        val bindings = mutable.ListBuffer.empty[Binding]
        perform {
            generateCalleeSave[Cont, Array[Instr]](calleeSaveRequired, this.codeGen, allocateRegisters(usedRegs)) |> {
                instrs += instructions.Halt
                finaliseRecs(recs)
                finaliseLets(bindings)
                generateHandlers(state.handlers)
                finaliseInstrs(instrs, state, recs.map(_._1), bindings.toList)
            }
        }
    }

    final private def generateHandlers(handlers: Iterable[(Instr, Int)])(implicit instrs: InstrBuffer): Unit = {
        for ((handler, label) <- handlers) {
            instrs += new instructions.Label(label)
            instrs += handler
        }
    }

    // This is a trick to get tail-calls to fire even in the presence of a legimate recursion
    @inline final protected def optimiseDefinitelyNotTailRec: StrictParsley[A] = optimise
    protected  [deepembedding] def optimise: StrictParsley[A] = this

    // Peephole optimisation and code generation - Top-down
    private [backend] def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit]
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
        // In a flatMap, that means a newly discovered global register must be allocated to a new slot
        // This should resize the register pool, but under current restrictions we'll just throw an
        // excepton if there are no available slots
        val unallocatedRegs = regs.filterNot(_.allocated)
        if (unallocatedRegs.nonEmpty) {
            val usedSlots = regs.collect {
                case reg if reg.allocated => reg.addr
            }
            val freeSlots = (0 until 4).filterNot(usedSlots)
            if (unallocatedRegs.size > freeSlots.size) {
                throw new IllegalStateException("Current restrictions require that the maximum number of registers in use is 4") // scalastyle:ignore throw
            }
            applyAllocation(unallocatedRegs, freeSlots)
        }
        else Nil
    }

    private def generateCalleeSave[Cont[_, +_], R](calleeSaveRequired: Boolean, bodyGen: =>Cont[R, Unit], allocatedRegs: List[Int])
                                                  (implicit ops: ContOps[Cont], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        if (calleeSaveRequired && allocatedRegs.nonEmpty) {
            val end = state.freshLabel()
            val calleeSave = state.freshLabel()
            instrs += new instructions.Label(calleeSave)
            instrs += new instructions.CalleeSave(end, allocatedRegs)
            bodyGen |> {
                instrs += new instructions.Jump(calleeSave)
                instrs += new instructions.Label(end)
            }
        }
        else bodyGen
    }

    private def finaliseRecs[Cont[_, +_]](recs: Iterable[(Rec[_], Cont[Unit, StrictParsley[_]])])
                                         (implicit ops: ContOps[Cont], instrs: InstrBuffer, state: CodeGenState): Unit = {
        for ((rec, p) <- recs) {
            instrs += new instructions.Label(rec.label)
            perform(p.flatMap(_.codeGen))
            instrs += instructions.Return
        }
    }

    private def finaliseLets[Cont[_, +_]](bindings: mutable.ListBuffer[Binding])
                                         (implicit ops: ContOps[Cont], instrs: InstrBuffer, state: CodeGenState): Unit = {
        while (state.more) {
            val let = state.nextLet()
            bindings += let
            instrs += new instructions.Label(let.label)
            perform[Cont, Unit](let.p.codeGen)
            instrs += instructions.Return
        }
    }

    // Applies tail-call optimisation:
    //   recursive bindings may tail-call to themselves or anything that doesn't require state-save
    //   other bindings may tail-call to anything that doesn't require state-save
    //   non-recursive bindings do not require state-save
    //   Call/GoSub replaced with Jump
    private def tco(instrs: Array[Instr], labels: Array[Int], bindings: List[Binding])(implicit state: CodeGenState): Unit = if (bindings.nonEmpty) {
        val bindingsWithReturns = bindings.zip(bindings.tail.map(_.location(labels) - 1) :+ (instrs.size-1))
        lazy val locToBinding = bindings.map(b => b.location(labels) -> b).toMap
        for ((binding, retLoc) <- bindingsWithReturns) instrs(retLoc-1) match {
            case instr: instructions.Call =>
                if (binding.isSelfCall(instr) || !locToBinding(instr.label).hasStateSave) {
                    instrs(retLoc-1) = new instructions.Jump(instr.label)
                }
            case instr: instructions.GoSub => instrs(retLoc-1) = new instructions.Jump(instr.label)
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
        PreservationAnalysis.determinePreserve(recs, instrs_)
        tco(instrs_, labelMapping, bindings)(state)
        instrs_
    }
}

private [backend] trait Binding {
    // When these are used by tco, the call instructions labels have already been shifted, but lets have not
    final def location(labelMap: Array[Int])(implicit state: CodeGenState): Int = this match {
        case self: Rec[_] => self.label
        case self: Let[_] => labelMap(self.label)
    }
    final def hasStateSave: Boolean = this match {
        case self: Rec[_] => self.preserve.nonEmpty
        case _: Let[_] => false
    }
    final def isSelfCall(call: instructions.Call): Boolean = this match {
        case self: Rec[_] => self.call == call
        case _: Let[_] => false
    }
}
private [deepembedding] trait MZero extends StrictParsley[Nothing]

// Internals
private [deepembedding] class CodeGenState {
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
    def getLabel(handler: Instr) = handlerMap.getOrElseUpdate(handler, freshLabel())
    def handlers: Iterable[(Instr, Int)] = handlerMap
}

package parsley.internal.deepembedding

import scala.language.{higherKinds, implicitConversions}
import scala.annotation.tailrec
import scala.collection.mutable

import parsley.BadLazinessException
import parsley.registers.Reg
import parsley.internal.machine.instructions, instructions.{Instr, JumpTable, Label}
import parsley.internal.ResizableArray
import Parsley.allocateRegisters
import ContOps.{safeCall, GenOps, perform, result, ContAdapter}

/**
  * This is the class that encapsulates the act of parsing and running an object of this class with `runParser` will
  * parse the string given as input to `runParser`.
  *
  * Note: In order to construct an object of this class you must use the combinators; the class itself is abstract
  *
  * @author Jamie Willis
  * @version 1
  */
private [parsley] abstract class Parsley[+A] private [deepembedding]
{
    final protected type InstrBuffer = ResizableArray[Instr]
    final protected type T = Any
    final protected type U = Any
    final protected type V = Any

    // $COVERAGE-OFF$
    final private [parsley] def prettyAST: String = {force(); safeCall((g: GenOps[String]) => perform(prettyASTAux(g))(g))}
    // $COVERAGE-ON$

    final def unsafe(): Unit = safe = false
     // $COVERAGE-OFF$
    final def force(): Unit = instrs
    final def overflows(): Unit = cps = true
     // $COVERAGE-ON$
    private [deepembedding] def demandCalleeSave(): this.type = {
        calleeSaveNeeded = true
        this
    }

    // Internals
    final private [deepembedding] def findLets[Cont[_, +_], R](implicit ops: ContOps[Cont, R], seen: Set[Parsley[_]], state: LetFinderState): Cont[R, Unit] = {
        state.addPred(this)
        if (seen(this)) result(state.addRec(this))
        else if (state.notProcessedBefore(this)) {
            this match {
                case self: UsesRegister => state.addReg(self.reg)
                case _ =>
            }

            try findLetsAux(ops, seen + this, state)
            catch {
                case npe: NullPointerException => throw new BadLazinessException
            }
        }
        else result(())
    }
    final private def applyLets[Cont[_, +_], R](implicit seen: Set[Parsley[_]], lets: LetMap, recs: RecMap): Parsley[A] = {
        // We use the seen set here to prevent cascading sub-routines
        val wasSeen = seen(this)
        val isLet = lets.contains(this)
        if (wasSeen && !isLet) recs(this)
        else if (wasSeen) this
        else if (isLet) lets(this)
        else this
    }
    final private [deepembedding] def optimised[Cont[_, +_], R, A_ >: A](implicit ops: ContOps[Cont, R], seen: Set[Parsley[_]],
                                                                                  lets: LetMap, recs: RecMap): Cont[R, Parsley[A_]] = {
        val fixed = this.applyLets
        val _seen = seen // Not needed in Scala 3, but really?!
        if (fixed.processed) result(fixed.optimise)
        else {
            implicit val seen: Set[Parsley[_]] = if (recs.contains(this) || lets.contains(this)) _seen + this else _seen
            for (p <- fixed.preprocess) yield p.optimise
        }
    }
    final private [deepembedding] var safe = true
    final private var cps = false
    final private [deepembedding] var size: Int = 1
    final private [deepembedding] var processed = false
    final private var calleeSaveNeeded = false

    final private def generateCalleeSave[Cont[_, +_], R](bodyGen: =>Cont[R, Unit], allocatedRegs: List[Int])
                                                        (implicit ops: ContOps[Cont, R], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit] = {
        if (calleeSaveNeeded && allocatedRegs.nonEmpty) {
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

    final private def pipeline[Cont[_, +_]](implicit ops: ContOps[Cont, Unit]): Array[Instr] ={
        implicit val instrs: InstrBuffer = new ResizableArray()
        implicit val state: CodeGenState = new CodeGenState
        implicit val letFinderState: LetFinderState = new LetFinderState
        implicit lazy val recMap: RecMap = new RecMap(letFinderState.recs, state)
        val bindings = mutable.ListBuffer.empty[Binding]
        perform {
            implicit val seenSet: Set[Parsley[_]] = Set.empty
            findLets >> {
                implicit val seenSet: Set[Parsley[_]] = letFinderState.recs
                implicit val usedRegs: Set[Reg[_]] = letFinderState.usedRegs
                implicit val letMap: LetMap = new LetMap(letFinderState.lets)
                optimised.flatMap(p => generateCalleeSave(p.codeGen, allocateRegisters(usedRegs))) |> {
                    instrs += instructions.Halt
                    finaliseRecs(bindings)
                    finaliseLets(bindings)
                }
            }
        }
        finaliseInstrs(instrs, state, recMap, bindings.toList)
    }

    final private def finaliseRecs[Cont[_, +_]](bindings: mutable.ListBuffer[Binding])(implicit ops: ContOps[Cont, Unit], instrs: InstrBuffer,
                                                                                                state: CodeGenState, lets: LetMap, recs: RecMap): Unit = {
        for (rec <- recs) {
            implicit val seenSet: Set[Parsley[_]] = recs.keys - rec.p
            bindings += rec
            instrs += new instructions.Label(rec.label)
            perform(rec.p.optimised.flatMap(_.codeGen))
            instrs += instructions.Return
        }
    }

    final private def finaliseLets[Cont[_, +_]](bindings: mutable.ListBuffer[Binding])(implicit ops: ContOps[Cont, Unit], instrs: InstrBuffer,
                                                                                                state: CodeGenState): Unit = {
        while (state.more) {
            val let = state.nextLet()
            bindings += let
            instrs += new instructions.Label(let.label)
            perform(let.p.codeGen)
            instrs += instructions.Return
        }
    }

    final private def computeInstrs(ops: GenOps[Unit]): Array[Instr] = pipeline(ops)

    final private def finaliseInstrs(instrs: InstrBuffer, state: CodeGenState, recs: Iterable[Rec[_]], bindings: List[Binding]): Array[Instr] = {
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

    // Applies tail-call optimisation:
    //   recursive bindings may tail-call to themselves or anything that doesn't require state-save
    //   other bindings may tail-call to anything that doesn't require state-save
    //   non-recursive bindings do not require state-save
    //   Call/GoSub replaced with Jump
    final private def tco(instrs: Array[Instr], labels: Array[Int], bindings: List[Binding])(implicit state: CodeGenState): Unit = if (bindings.nonEmpty) {
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

    final private [parsley] lazy val instrs: Array[Instr] = if (cps) computeInstrs(Cont.ops.asInstanceOf[GenOps[Unit]]) else safeCall(computeInstrs(_))
    final private lazy val pindices: Array[Int] = instructions.statefulIndices(instrs)
    final private [parsley] def threadSafeInstrs: Array[Instr] = instructions.stateSafeCopy(instrs, pindices)

    // This is a trick to get tail-calls to fire even in the presence of a legimate recursion
    final private [deepembedding] def optimiseDefinitelyNotTailRec: Parsley[A] = optimise

    // Abstracts
    // Sub-tree optimisation and Rec calculation - Bottom-up
    protected def preprocess[Cont[_, +_], R, A_ >: A](implicit ops: ContOps[Cont, R], seen: Set[Parsley[_]], lets: LetMap, recs: RecMap): Cont[R, Parsley[A_]]
    // Let-finder recursion
    protected def findLetsAux[Cont[_, +_], R](implicit ops: ContOps[Cont, R], seen: Set[Parsley[_]], state: LetFinderState): Cont[R, Unit]
    // Optimisation - Bottom-up
    protected def optimise: Parsley[A] = this
    // Peephole optimisation and code generation - Top-down
    private [parsley] def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont, R], instrs: InstrBuffer, state: CodeGenState): Cont[R, Unit]
    private [parsley] def prettyASTAux[Cont[_, +_], R](implicit ops: ContOps[Cont, R]): Cont[R, String]
}
private [deepembedding] object Parsley {
    private def applyAllocation(regs: Set[Reg[_]], freeSlots: Iterable[Int]): List[Int] = {
        val allocatedSlots = mutable.ListBuffer.empty[Int]
        for ((reg, addr) <- regs.zip(freeSlots)) {
            reg.allocate(addr)
            allocatedSlots += addr
        }
        allocatedSlots.toList
    }

    private [Parsley] def allocateRegisters(regs: Set[Reg[_]]): List[Int] = {
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
                throw new IllegalStateException("Current restrictions require that the maximum number of registers in use is 4")
            }
            applyAllocation(unallocatedRegs, freeSlots)
        }
        else Nil
    }
}

private [deepembedding] trait Binding {
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
private [deepembedding] trait MZero extends Parsley[Nothing]
private [deepembedding] trait UsesRegister {
    val reg: Reg[_]
}

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
}

private [deepembedding] class LetFinderState {
    private val _recs = mutable.Set.empty[Parsley[_]]
    private val _preds = mutable.Map.empty[Parsley[_], Int]
    private val _usedRegs = mutable.Set.empty[Reg[_]]

    def addPred(p: Parsley[_]): Unit = _preds += p -> (_preds.getOrElseUpdate(p, 0) + 1)
    def addRec(p: Parsley[_]): Unit = _recs += p
    def addReg(reg: Reg[_]): Unit = _usedRegs += reg
    def notProcessedBefore(p: Parsley[_]): Boolean = _preds(p) == 1

    def lets: Iterable[Parsley[_]] = _preds.toSeq.view.collect {
        case (p, refs) if refs >= 2 && !_recs(p) => p
    }
    lazy val recs: Set[Parsley[_]] = _recs.toSet
    def usedRegs: Set[Reg[_]] = _usedRegs.toSet
}

private [deepembedding] abstract class ParserMap[V[_] <: Parsley[_]](ks: Iterable[Parsley[_]]) {
    protected def make(p: Parsley[_]): V[_]
    protected val map: Map[Parsley[_], V[_]] = ks.map(p => p -> make(p)).toMap
    val keys: Set[Parsley[_]] = ks.toSet
    def contains(p: Parsley[_]): Boolean = keys(p)
    def apply[A](p: Parsley[A]): V[A] = map(p).asInstanceOf[V[A]]
    override def toString: String = map.toString
}

private [deepembedding] class LetMap(lets: Iterable[Parsley[_]]) extends ParserMap[Let](lets) {
    def make(p: Parsley[_]): Let[_] = new Let(p)
}

private [deepembedding] class RecMap(recs: Iterable[Parsley[_]], state: CodeGenState) extends ParserMap[Rec](recs) with Iterable[Rec[_]] {
    def make(p: Parsley[_]): Rec[_] = new Rec(p, new instructions.Call(state.freshLabel()))
    override def iterator: Iterator[Rec[_]] = map.values.iterator
}
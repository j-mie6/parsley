package parsley.internal.deepembedding

import scala.language.{higherKinds, implicitConversions}
import scala.annotation.tailrec
import scala.collection.mutable

import parsley.BadLazinessException
import parsley.registers.Reg
import parsley.internal.machine.instructions, instructions.{Instr, JumpTable, Label}
import parsley.internal.ResizableArray
import ContOps.{safeCall, GenOps, perform, result, ContAdapter}

import backend.StrictParsley

/**
  * This is the class that encapsulates the act of parsing and running an object of this class with `runParser` will
  * parse the string given as input to `runParser`.
  *
  * Note: In order to construct an object of this class you must use the combinators; the class itself is abstract
  *
  * @author Jamie Willis
  * @version 1
  */
private [parsley] abstract class Parsley[+A] private [deepembedding] //extends StrictParsley[A]
{
    // $COVERAGE-OFF$
    final private [parsley] def prettyAST: String = {force(); safeCall((g: GenOps[String]) => perform(prettyASTAux(g))(g))}
    // $COVERAGE-ON$

    // $COVERAGE-OFF$
    final def unsafe(): Unit = safe = false
    final def force(): Unit = instrs
    final def overflows(): Unit = cps = true
    // $COVERAGE-ON$
    private [deepembedding] def demandCalleeSave(): this.type = {
        calleeSaveNeeded = true
        this
    }

    // Internals
    final private [deepembedding] def findLets[Cont[_, +_], R](seen: Set[Parsley[_]])(implicit ops: ContOps[Cont, R], state: LetFinderState): Cont[R, Unit] = {
        state.addPred(this)
        if (seen(this)) result(state.addRec(this))
        else if (state.notProcessedBefore(this)) {
            this match {
                case self: UsesRegister => state.addReg(self.reg)
                case _ =>
            }

            try findLetsAux(seen + this)
            catch {
                // $COVERAGE-OFF$
                case npe: NullPointerException => throw new BadLazinessException
                // $COVERAGE-ON$
            }
        }
        else result(())
    }
    final private def applyLets[Cont[_, +_], R](implicit lets: LetMap, recs: RecMap): Either[Parsley[A], StrictParsley[A]] = {
        if (recs.contains(this)) Right(recs(this))
        else if (lets.contains(this)) Right(lets(this))
        else Left(this)
    }
    final private [deepembedding] def optimised[Cont[_, +_], R, A_ >: A](implicit ops: ContOps[Cont, R],
                                                                                  lets: LetMap, recs: RecMap): Cont[R, StrictParsley[A_]] = {
        if (recs.contains(this)) result(recs(this))
        else if (lets.contains(this)) result(lets(this))
        else this.unsafeOptimised
    }
    final private [deepembedding] def unsafeOptimised[Cont[_, +_], R, A_ >: A](implicit ops: ContOps[Cont, R],
                                                                                        lets: LetMap, recs: RecMap): Cont[R, StrictParsley[A_]] = {
        for (p <- this.preprocess) yield p.optimise
    }
    final private [deepembedding] var safe = true
    final private var cps = false
    final private [deepembedding] var calleeSaveNeeded = false

    final private def pipeline[Cont[_, +_]](implicit ops: ContOps[Cont, Array[Instr]]): Array[Instr] ={
        implicit val state: backend.CodeGenState = new backend.CodeGenState
        implicit val letFinderState: LetFinderState = new LetFinderState
        perform {
            findLets(Set.empty) >> {
                val usedRegs: Set[Reg[_]] = letFinderState.usedRegs
                implicit val seenSet: Set[Parsley[_]] = letFinderState.recs
                implicit val recMap: RecMap = RecMap(letFinderState.recs)
                implicit val letMap: LetMap = LetMap(letFinderState.lets)(ops.asInstanceOf[ContOps[Cont, StrictParsley[_]]], recMap)
                val recs_ = recMap.map { case (p, strict) =>
                    // Pretty uggo, ngl.
                    implicit val _ops: ContOps[Cont, Unit] = ops.asInstanceOf[ContOps[Cont, Unit]]
                    (strict, p.unsafeOptimised[Cont, Unit, Any])
                }
                for (sp <- this.optimised) yield {
                    sp.cps = cps
                    sp.safe = safe
                    sp.generateInstructions(calleeSaveNeeded, usedRegs, recs_)
                }
            }
        }
    }

    final private def computeInstrs(ops: GenOps[Array[Instr]]): Array[Instr] = pipeline(ops)

    final private [parsley] lazy val instrs: Array[Instr] = if (cps) computeInstrs(Cont.ops.asInstanceOf[GenOps[Array[Instr]]]) else safeCall(computeInstrs(_))
    final private lazy val pindices: Array[Int] = instructions.statefulIndices(instrs)
    final private [parsley] def threadSafeInstrs: Array[Instr] = instructions.stateSafeCopy(instrs, pindices)

    // Abstracts
    // Sub-tree optimisation and Rec calculation - Bottom-up
    private [deepembedding] def preprocess[Cont[_, +_], R, A_ >: A](implicit ops: ContOps[Cont, R], lets: LetMap, recs: RecMap): Cont[R, StrictParsley[A_]]
    // Let-finder recursion
    protected def findLetsAux[Cont[_, +_], R](seen: Set[Parsley[_]])(implicit ops: ContOps[Cont, R], state: LetFinderState): Cont[R, Unit]
    private [parsley] def prettyASTAux[Cont[_, +_], R](implicit ops: ContOps[Cont, R]): Cont[R, String]
}

private [deepembedding] trait UsesRegister {
    val reg: Reg[_]
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

private [deepembedding] final class LetMap(letGen: Map[Parsley[_], LetMap => StrictParsley[_]]) {
    // This might not necessarily contain Let nodes: if they were inlined then they will not be present here
    private val mutMap = mutable.Map.empty[Parsley[_], StrictParsley[_]]

    def contains(p: Parsley[_]): Boolean = letGen.contains(p)
    def apply[A](p: Parsley[A]): StrictParsley[A] = mutMap.getOrElseUpdate(p, {
        val sp = letGen(p)(this)
        if (sp.inlinable) sp else new backend.Let(sp)
    }).asInstanceOf[StrictParsley[A]]
    override def toString: String = mutMap.toString
}
private [deepembedding] object LetMap {
    def apply[Cont[_, +_]](lets: Iterable[Parsley[_]])(implicit ops: ContOps[Cont, StrictParsley[_]], recs: RecMap): LetMap = {
        new LetMap(lets.map(p => p -> ((_self: LetMap) => {
            implicit val self: LetMap = _self
            perform(p.unsafeOptimised)
        })).toMap)
    }
}

private [deepembedding] final class RecMap(map: Map[Parsley[_], backend.Rec[_]]) extends Iterable[(Parsley[_], backend.Rec[_])] {
    def contains(p: Parsley[_]): Boolean = map.contains(p)
    def apply[A](p: Parsley[A]): backend.Rec[A] = map(p).asInstanceOf[backend.Rec[A]]
    override def toString: String = map.toString
    override def iterator: Iterator[(Parsley[_], backend.Rec[_])] = map.iterator
}
private [deepembedding] object RecMap {
    def apply(recs: Iterable[Parsley[_]])(implicit state: backend.CodeGenState): RecMap = {
        new RecMap(recs.map(p => p -> new backend.Rec(new instructions.Call(state.freshLabel()))).toMap)
    }
}
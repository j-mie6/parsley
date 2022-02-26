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
    final private def applyLets[Cont[_, +_], R](implicit seen: Set[Parsley[_]], lets: LetMap, recs: RecMap): Either[Parsley[A], StrictParsley[A]] = {
        // We use the seen set here to prevent cascading sub-routines
        val wasSeen = seen(this)
        val isLet = lets.contains(this)
        if (wasSeen && !isLet) Right(recs(this).strict)
        else if (wasSeen) Left(this)
        else if (isLet) Left(this)//Right(lets(this)) TODO: Renable
        else Left(this)
    }
    final private [deepembedding] def optimised[Cont[_, +_], R, A_ >: A](implicit ops: ContOps[Cont, R], seen: Set[Parsley[_]],
                                                                                  lets: LetMap, recs: RecMap): Cont[R, StrictParsley[A_]] = {
        val fixed = this.applyLets
        val _seen = seen // Not needed in Scala 3, but really?!
        fixed match {
            case Right(p) => result(p.optimise)
            case Left(p) =>
                implicit val seen: Set[Parsley[_]] = if (recs.contains(this) || lets.contains(this)) _seen + this else _seen
                for (p <- p.preprocess) yield p.optimise
        }
    }
    final private [deepembedding] var safe = true
    final private var cps = false
    final private [deepembedding] var calleeSaveNeeded = false

    final private def pipeline[Cont[_, +_]](implicit ops: ContOps[Cont, Array[Instr]]): Array[Instr] ={
        implicit val state: backend.CodeGenState = new backend.CodeGenState
        implicit val letFinderState: LetFinderState = new LetFinderState
        perform {
            findLets(Set.empty) >> {
                implicit val seenSet: Set[Parsley[_]] = letFinderState.recs
                implicit val recMap: RecMap = new RecMap(letFinderState.recs, state)
                implicit val letMap: LetMap = new LetMap(letFinderState.lets)
                val usedRegs: Set[Reg[_]] = letFinderState.usedRegs
                val recs_ = recMap.map { rec =>
                    // Pretty uggo, ngl.
                    implicit val seenSet: Set[Parsley[_]] = recMap.keys - rec.p
                    implicit val _ops: ContOps[Cont, Unit] = ops.asInstanceOf[ContOps[Cont, Unit]]
                    (rec.strict, rec.p.optimised[Cont, Unit, Any])
                }
                // TODO: I want all the lets processed and populated HERE
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
    protected def preprocess[Cont[_, +_], R, A_ >: A](implicit ops: ContOps[Cont, R], seen: Set[Parsley[_]], lets: LetMap, recs: RecMap): Cont[R, StrictParsley[A_]]
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

private [deepembedding] class LetMap(lets: Iterable[Parsley[_]]) {
    def make(p: Parsley[_]): backend.Let[_] = null//new backend.Let(p) TODO: Renable
    protected val map: Map[Parsley[_], backend.Let[_]] = lets.map(p => p -> make(p)).toMap
    val keys: Set[Parsley[_]] = lets.toSet
    def contains(p: Parsley[_]): Boolean = keys(p)
    def apply[A](p: Parsley[A]): backend.Let[A] = map(p).asInstanceOf[backend.Let[A]]
    override def toString: String = map.toString
}

private [deepembedding] class RecMap(recs: Iterable[Parsley[_]], state: backend.CodeGenState) extends Iterable[Rec[_]] {
    def make(p: Parsley[_]): Rec[_] = new Rec(p, new backend.Rec(new instructions.Call(state.freshLabel())))
    protected val map: Map[Parsley[_], Rec[_]] = recs.map(p => p -> make(p)).toMap
    val keys: Set[Parsley[_]] = recs.toSet
    def contains(p: Parsley[_]): Boolean = keys(p)
    def apply[A](p: Parsley[A]): Rec[A] = map(p).asInstanceOf[Rec[A]]
    override def toString: String = map.toString
    override def iterator: Iterator[Rec[_]] = map.values.iterator
}
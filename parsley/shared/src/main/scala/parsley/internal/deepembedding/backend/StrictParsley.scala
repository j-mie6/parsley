/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.backend

import scala.annotation.tailrec
import scala.collection.mutable

import parsley.XAssert._
import parsley.exceptions.CorruptedReferenceException
import parsley.state.Ref

import parsley.internal.collection.mutable.ResizableArray
import parsley.internal.deepembedding.ContOps, ContOps.{perform, ContAdapter}
import parsley.internal.machine.instructions, instructions.{Instr, Label}

import StrictParsley.*

/** This is the root type of the parsley "backend": it represents a combinator tree
  * where the join-points in the tree (recursive or otherwise) have been factored into
  * `Let` and `Rec` nodes. This means the tree is finite and acyclic.
  *
  * @note objects of this type are freshly generated by the frontend, so can be mutable
  *       and are allowed to pull all sorts of nasty tricks, as they are guaranteed to
  *       be thread- and parser-local
  */
private [deepembedding] trait StrictParsley[+A] {
    /** This function forms the entry-point of the "backend" of parsley's compiler.
      *
      * The process is as follows:
      *   1. if this parser was generated by a `flatMap`, it may need to generate a `CalleeSave` instruction
      *   1. code generation is performed for the main (non-shared) body of the parser
      *   1. a `Halt` instruction is generated to signify the end of the parser
      *   1. all the shared parsers are then generated terminated by `Return` instructions
      *   1. any sharable, fail-only, handler instructions are then generated at the end of the instruction stream
      *   1. jump-labels in the code are removed, and tail-call optimisation is applied
      *
      * @param minRef the number of references determined to currently exist according to the context (or -1 if this is root)
      * @param usedRefs the references used by this parser (these may require allocation)
      * @param recs a stream of pairs of rec nodes and the generators for their strict parsers
      *             (this is just because more `Cont` operations are performed later)
      * @param state the code generator state
      * @return the final array of instructions for this parser
      */
    final private [deepembedding] def generateInstructions[M[_, +_]: ContOps](minRef: Int, usedRefs: Set[Ref[_]], bodyMap: Map[Let[_], StrictParsley[_]])
                                                                            (implicit state: CodeGenState): Array[Instr] = {
        implicit val instrs: InstrBuffer = newInstrBuffer
        perform {
            allocateAndExpandRefs(minRef, usedRefs)
            this.codeGen[M, Array[Instr]](producesResults = true) |> {
                // When `minRef` is -1 this is top level, otherwise it is a flatMap
                instrs += (if (minRef >= 0) instructions.Return else instructions.Halt)
                val letRets = finaliseLets(bodyMap)
                generateHandlers(state.handlers)
                finaliseInstrs(instrs, state.nlabels, letRets)
            }
        }
    }

    /** This method performs the code generation for this combinator and the recursive sub-parsers.
      *
      * It is fine for this method to perform peephole optimisation on the combinators and generate
      * more optimal sequences of instructions in specific circumstances.
      *
      * @param producesResults is this parser expected to push its result onto the stack?
      * @param instrs the current buffer of instructions to generate into
      * @param state code generator state, for the generation of labels
      */
    protected [backend] def codeGen[M[_, +_]: ContOps, R](producesResults: Boolean)(implicit instrs: InstrBuffer, state: CodeGenState): M[R, Unit]

    /** This method is directly called by the "frontend" and is used to perform domain-specific
      * optimisations on this parser (usually following the laws of the parser combinators).
      *
      * By default, this method just returns this parser unchanged.
      */
    protected [deepembedding] def optimise: StrictParsley[A] = this

    /** Should this parser be inlined if it is shared?
      *
      * @note the heuristic here is that single instruction combinators should always be inlined
      */
    private [deepembedding] def inlinable: Boolean

    // $COVERAGE-OFF$
    /** Pretty-prints a combinator tree, for internal debugging purposes only. */
    private [deepembedding] def pretty: String
    // $COVERAGE-ON$
}

private [deepembedding] object StrictParsley {
    /** The kind of buffer that should be used to generate instructions into */
    private [deepembedding] type InstrBuffer = ResizableArray[Instr]
    /** The type of a return label */
    private type RetLoc = Int

    /** Make a fresh instruction buffer */
    private def newInstrBuffer: InstrBuffer = new ResizableArray()

    /** Allocates references, and, if required, generates an instruction to expand array size.
      *
      * This is needed when using `flatMap`, as it is unaware of the register
      * context of its parents.
      *
      * @param minRef the number of references in existance, according to the context
      * @param usedRefs the referenced used in this parser that may need allocation
      * @param instrs the instruction buffer
      */
    private def allocateAndExpandRefs(minRef: Int, usedRefs: Set[Ref[_]])(implicit instrs: InstrBuffer): Unit = {
        val blockedSlots = usedRefs.collect {
            case r if r.allocated => r.addr
        }
        @tailrec
        def nextFreeSlot(n: Int): Int = if (blockedSlots.contains(n)) nextFreeSlot(n+1) else n

        var nextSlot = nextFreeSlot(math.max(minRef, 0))
        for (r <- usedRefs if !r.allocated) {
            r.allocate(nextSlot)
            nextSlot = nextFreeSlot(nextSlot+1)
        }
        // check that no two references have the same address!
        if (usedRefs.groupBy(_.addr).valuesIterator.exists(_.size > 1)) {
            throw new CorruptedReferenceException() // scalastyle:ignore throw
        }
        val totalSlotsRequired = nextSlot
        // if this is -1, then we are the top level and have no parent, otherwise it needs to be done
        if (minRef >= 0 && (minRef < totalSlotsRequired)) {
            instrs += new instructions.ExpandRefs(totalSlotsRequired)
        }
    }

    /** Generates each of the shared, non-recursive, parsers that have been ''used'' by
      * the parser. These are stored within the code generation state. This is done because
      * some of the identified shared parsers may have been inlined and so do not need to
      * be generated. The state tracks which of these parsers were actually demanded, so
      * dead-code is automatically eliminated.
      *
      * @param bodyMap the map of lets to bodies
      * @param instrs the instruction buffer to generate into
      * @param state the code generation state, which contains the shared parsers
      * @return the list of return labels for each of the parsers (for TCO)
      */
    private def finaliseLets[M[_, +_]: ContOps](bodyMap: Map[Let[_], StrictParsley[_]])(implicit instrs: InstrBuffer, state: CodeGenState): List[RetLoc] = {
        val retLocs = mutable.ListBuffer.empty[RetLoc]
        while (state.more) {
            val (let, producesResults, label) = state.nextLet()
            instrs += new instructions.Label(label)
            perform[M, Unit](bodyMap(let).codeGen(producesResults))
            val retLoc = state.freshLabel()
            instrs += new instructions.Label(retLoc)
            instrs += instructions.Return
            retLocs += retLoc
        }
        retLocs.toList
    }

    /** Generates each of the shared handlers that have been demanded by the parser.
      *
      * These are those handlers that always fail, and have no combinator specific
      * (or rather, non-sharable) configuration. This means that they can be generated
      * once at the end of the instruction buffer and jumped to from anywhere:
      * they will always fail and jump-elsewhere anyway.
      *
      * @param handlers the source of handlers, and their raw labels
      * @param instrs the instruction buffer to generate into
      */
    private def generateHandlers(handlers: Iterator[(Instr, Int)])(implicit instrs: InstrBuffer): Unit = {
        for ((handler, label) <- handlers) {
            instrs += new instructions.Label(label)
            instrs += handler
        }
    }

    /** This function takes the fully generated instruction buffer (complete with labels) and
      * processes it into the final array of instructions.
      *
      * The processing happens in stages:
      *   1. traverse the buffer and find the true offset of each label in the instruction buffer
      *   1. allocate a new instruction array of the correct size
      *   1. fill this array from the original buffer by dropping each label, and resolve labels in instructions
      *   1. perform tail-call optimisation
      *
      * @param instrs the final instruction buffer
      * @param numLabels the number of labels within the instruction buffer
      * @param retLocs the labels that point to return instructions within the instruction buffer (for TCO)
      * @return the final array of instructions
      */
    private def finaliseInstrs(instrs: InstrBuffer, numLabels: Int, retLocs: List[RetLoc]): Array[Instr] = {
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
        val labelMapping = new Array[Int](numLabels)
        val size = findLabels(instrsOversize, labelMapping, instrs.length, 0, 0)
        val instrs_ = new Array[Instr](size)
        applyLabels(instrsOversize, labelMapping, instrs_, instrs_.length, 0, 0)
        tco(instrs_, labelMapping, retLocs)
        instrs_
    }

    /** Performs Tail-Call Optimisation (TCO) on the final array of instructions.
      *
      * This is done by checking the instruction before each `Return` instruction in the
      * instruction buffer. If this is a `Call` instruction, then this is a tail-call
      * (where the last thing the subroutine does is call another subroutine). In this
      * case, the callstack frame can reused and the `Call` can be replaced by a `Jump`.
      *
      * @param instrs the instruction array
      * @param labelMapping the mapping from labels to their corresponding offset in the array
      * @param retLocs the list of labels that should be checked for tail-call
      */
    private def tco(instrs: Array[Instr], labelMapping: Array[Int], retLocs: List[RetLoc]): Unit = {
        for (label <- retLocs) {
            val retLoc = labelMapping(label)
            assert(instrs(retLoc) eq instructions.Return, "return locations are actually `Return`s")
            instrs(retLoc-1) match {
                case instr: instructions.Call => instrs(retLoc-1) = new instructions.Jump(instr.label)
                case _ =>
            }
        }
    }
}

/** Denotes that a parser unconditionally fails. */
private [deepembedding] trait MZero extends StrictParsley[Nothing]

/** This is the escapulated state required for code generation,
  * which is threaded through the entire backend.
  *
  * @param numRefs the number of references required by the parser being generated
  */
private [deepembedding] class CodeGenState(val numRefs: Int) {
    /** The next jump-label identifier. */
    private var current = 0
    /** The shared-parsers that have been referenced at some point in the generation so far. */
    private val queue = mutable.ListBuffer.empty[(Let[_], Boolean, Int)]
    /** The mapping between a shared-parser and its generated jump-label. */
    private val map = mutable.Map.empty[(Let[_], Boolean), Int]

    /** Generates a unique jump-label. */
    def freshLabel(): Int = {
        val next = current
        current += 1
        next
    }
    /** Returns the number of labels generated by the code generation process */
    def nlabels: Int = current

    /** Given a shared-parser, will return the assigned jump-label for it,
      * generating one should it not exist yet.
      *
      * @param sub the shared parser to collect a label for
      * @return the label assigned the given parser
      */
    def getLabel(sub: Let[_], producesResults: Boolean): Int = map.getOrElseUpdate((sub, producesResults), {
        val label = freshLabel()
        (sub, producesResults, label) +=: queue
        label
    })

    /** Returns the next shared-parser that has been refered during code generation */
    def nextLet(): (Let[_], Boolean, Int) = queue.remove(0)
    /** Are there any more shared-parsers left on the processing queue? */
    def more: Boolean = queue.nonEmpty

    // Handler caching
    // To reduce redundant instructions appearing in the instruction array, many of the
    // handler instructions in parsley can be cached and generated in a single place.
    // This is because handler instructions are always jumped to (never executed by
    // fall-through) and these handlers specifically always continue by failing (which
    // means their location in the array is irrelevant). However, there are several
    // such handlers, and some of them have some configuration, like `RelabelError`
    // but this may also be shared across many parts of the parser. This gives rise
    // to several handler maps within the state

    /** A map of generic handler instructions to some assigned jump-label. */
    private val handlerMap = mutable.Map.empty[Instr, Int]
    /** A map of error labels to the assigned jump-label to its instruction. */
    private val relabelErrorMap = mutable.Map.empty[scala.Seq[String], Int]
    /** A map of reasons to the assigned jump-label to its instruction. */
    private val applyReasonMap = mutable.Map.empty[String, Int]
    /** A map of registers to the assigned jump-label to its instruction. */
    private val putAndFailMap = mutable.Map.empty[Ref[_], Int]
    /** A map of dislodge amounts to the assigned jump-label to its instruction. */
    private val dislodgeAndFailMap = mutable.Map.empty[Int, Int]

    /** Given a generic handler instruction, fetch the corresponding jump-label
      * that will represent it in the final instruction array.
      */
    def getLabel(handler: Instr): Int  = handlerMap.getOrElseUpdate(handler, freshLabel())
    /** Given a error label, fetch the corresponding jump-label that will represent
      * the `RelabelError(label)` instruction in the final instruction array.
      */
    def getLabelForRelabelError(labels: scala.Seq[String]): Int = relabelErrorMap.getOrElseUpdate(labels, freshLabel())
    /** Given an error reason, fetch the corresponding jump-label that will represent
      * the `ApplyReason(reason)` instruction in the final instruction array.
      */
    def getLabelForApplyReason(reason: String): Int = applyReasonMap.getOrElseUpdate(reason, freshLabel())
    /** Given a register, fetch the corresponding jump-label that will represent
      * the `PutAndFail(reg)` instruction in the final instruction array.
      */
    def getLabelForPutAndFail(reg: Ref[_]): Int = putAndFailMap.getOrElseUpdate(reg, freshLabel())
    /** Given an amount to dislodge, fetch the corresponding jump-label that will represent
      * the `DislodgeAndFail(reg)` instruction in the final instruction array.
      */
    def getLabelForDislodgeAndFail(n: Int): Int = dislodgeAndFailMap.getOrElseUpdate(n, freshLabel())

    /** An iterator over all the handler instructions and their corresponding jump-labels that have
      * been demanded during the process of code-generation.
      */
    def handlers: Iterator[(Instr, Int)] = {
        val relabelErrors = relabelErrorMap.view.map {
            case (labels, i) => new instructions.RelabelErrorAndFail(labels) -> i
        }
        val applyReasons = applyReasonMap.view.map {
            case (reason, i) => new instructions.ApplyReasonAndFail(reason) -> i
        }
        val putAndFail = putAndFailMap.view.map {
            case (reg, i) => new instructions.PutAndFail(reg.addr) -> i
        }
        val dislodgeAndFail = dislodgeAndFailMap.view.map {
            case (n, i) => new instructions.DislodgeAndFail(n) -> i
        }
        new Iterator[(Instr, Int)] {
            private var rest = List(relabelErrors.iterator, applyReasons.iterator, putAndFail.iterator, dislodgeAndFail.iterator)
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

/* SPDX-FileCopyrightText: © 2021 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.errors

import parsley.XAssert._

import parsley.internal.errors.{Desc, ExpectItem}

/** This structure represents a collection of operations that can be performed
  * between `List[Set[ErrorItem]]`, which are known as `Hints`. Each set in the
  * structure is sourced from an error message directly, and the list allows
  * for these error messages to be kept independent. The collapsing of the
  * hints structure will not yield this list, however, and will produce a
  * final set of error items.
  *
  * @param size the number of hint sets represented by this structure
  */
private [machine] sealed abstract class DefuncHints(private [errors] val size: Int) {
    // this is a neat trick to ensure that no reprocessing of hints needs to occur
    // if they appear at a different point in the error message: if part of the hint-structure
    // is skipped over, then this value tells us up to what point we need to provide the
    // remaining elements. This obviously starts with all the elements; then if some
    // is skipped over, we know the remaining ones are already incorporated and don't need
    // to be handled again: this is ok, because hints cannot be duplicated as it is a Set.
    private var incorporatedAfter: Int = size
    private [errors] def nonEmpty: Boolean = size != 0
    private [errors] def isEmpty: Boolean = size == 0
    /** This function evaluates this `DefuncHints` structure into the actual set of
      * error items it represents and adds this directly into the provided `TrivialErrorBuilder`
      *
      * @note this function is ''impure'' and changes properties of the hints themselves
      */
    private [machine] def updateExpectedsAndGetSize(builder: TrivialErrorBuilder): Int = {
        val hintCollector = builder.makeHintCollector
        collect(0, hintCollector)
        hintCollector.unexpectWidth
    }
    /** This function evaulates this `DefuncHints` structure into an actual set of
      * error items independently of any error messages.
      *
      * @note this function is ''pure'' and can be used at will
      */
    private [machine] def toSet: Set[ExpectItem] = {
        val state: HintCollector = new HintCollector
        collect(0, state)
        // this /must/ be done to ensure that this function is pure, as `collect` can alter this value.
        reset()
        state.mkSet
    }
    /** This function undoes the mutation that is performed by `updateExpectedsAndGetSize` allowing
      * this hint to be used again.
      */
    private [machine] def reset(): Unit = {
        resetDeep()
        resetThis()
    }
    private def resetThis(): Unit = incorporatedAfter = size
    protected def resetDeep(): Unit
    final private [errors] def collect(skipNext: Int, collector: HintCollector): Unit = if (skipNext < incorporatedAfter) {
        // This error only needs to provide the first `skipNext` elements if we encounter it again
        incorporatedAfter = skipNext
        this match {
            case EmptyHints =>
            // Popping and replacing are both achieved by skipping the next hint to be processed
            case self: PopHints => self.hints.collect(skipNext + 1, collector)
            case self: ReplaceHint =>
                // replacing a hint already skips on its own anyway, so this is like skipNext - 1 + 1 == skipNext
                if (skipNext > 0) self.hints.collect(skipNext, collector)
                else {
                    collector += Desc(self.label)
                    self.hints.collect(skipNext + 1, collector)
                }
            case self: MergeHints =>
                // if there are less hints in the first set than we want to skip, then it can be hopped over
                if (self.oldHints.size <= skipNext) self.newHints.collect(skipNext - self.oldHints.size, collector)
                else {
                    // otherwise, we know that all the hints to skip are in the first set (with some which are needed!),
                    // and we can travese the second from the beginning
                    self.oldHints.collect(skipNext, collector)
                    self.newHints.collect(0, collector)
                }
            case self: AddError =>
                // skipping happens inside out here: the hints structure is a snoc-list
                // so first self.hints should be processed /technically/, however that would make this non-tail recursive
                // Good news is that the order the hints are /contributed/ in doesn't matter, because it's a set
                // so the order can be reversed.
                // We incorporate this error only when self.hints absorbed all the skips (skipNext <= self.hints.size)
                if (skipNext < self.size) self.err.collectHints(collector)
                self.hints.collect(skipNext, collector)
        }
    }

    // Operations: these are the smart constructors for the hint operations, which will reduce the number of objects in the binary
    // they all perform some form of simplification step to avoid unnecesary allocations

    /** This operation is used by the `hide` combinator to remove the first
      * set of hints currently in-flight. This is explicitly following the
      * behaviour of megaparsec.
      */
    private [machine] final def pop: DefuncHints = if (size > 1) new PopHints(this) else EmptyHints
    /** This operation is used by the `label` combinator to rename the first
      * set of hints currently in-flight. This is explicitly following the
      * behaviour of megaparsec.
      *
      * @param label the name to replace the first set of hints with
      */
    private [machine] final def rename(label: String): DefuncHints = if (nonEmpty) new ReplaceHint(label, this) else this
    /** This operation merges two sets of hints together. This used by `label`
      * to combine the saved hints with those that may have been generated and
      * affected by the label. This is not like a set-union, however, and is
      * more like a `++` on the list of sets from each error.
      *
      * @param newHints the hints to merge into these ones
      */
    private [machine] final def merge(newHints: DefuncHints): DefuncHints = {
        if (this.isEmpty) newHints
        else if (newHints.isEmpty) this
        else new MergeHints(this, newHints)
    }
    /** This represents the snocing of a new set of hints onto the existing list of
      * sets. This is used whenever an error message is discarded by a parser succeeding
      * again.
      *
      * @param err the set of error items to incorporate, represented in uncomputed form as `DefuncError`
      */
    private [machine] final def addError(err: DefuncError): DefuncHints = {
        assume(err.isTrivialError, "only trivial errors will get added to the hints")
        new AddError(this, err.asInstanceOf[TrivialDefuncError])
    }
}

/** Represents no hints at all. */
private [machine] object EmptyHints extends DefuncHints(size = 0) {
    def resetDeep(): Unit = ()
}

private [machine] final class PopHints private [errors] (val hints: DefuncHints) extends DefuncHints(size = hints.size - 1) {
    def resetDeep(): Unit = hints.reset()
}

private [errors] final class ReplaceHint private [errors] (val label: String, val hints: DefuncHints) extends DefuncHints(size = hints.size) {
    assume(label.nonEmpty)
    def resetDeep(): Unit = hints.reset()
}

private [errors] final class MergeHints private [errors] (val oldHints: DefuncHints, val newHints: DefuncHints)
    extends DefuncHints(size = oldHints.size + newHints.size) {
    def resetDeep(): Unit = {
        oldHints.reset()
        newHints.reset()
    }
}

private [machine] final class AddError private [errors] (val hints: DefuncHints, val err: TrivialDefuncError) extends DefuncHints(size = hints.size + 1) {
    def resetDeep(): Unit = {
        hints.reset()
        err.resetHints()
    }
}

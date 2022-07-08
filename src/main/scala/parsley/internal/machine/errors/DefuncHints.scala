/* SPDX-FileCopyrightText: © 2021 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.errors

import parsley.internal.errors.{Desc, ErrorItem}

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
      * error items it represents and adds this directly into the provided `TrivialState`
      *
      * @note this function is ''impure'' and changes properties of the hints themselves
      */
    private [machine] def updateExpectedsAndGetSize(state: TrivialState): Int = {
        val hintState = state.asHintState
        collect(0, hintState)
        hintState.unexpectSize
    }
    private [machine] def toSet: Set[ErrorItem] = {
        val state: HintState = new HintState
        collect(0, state)
        // this /must/ be set to ensure that this function is pure, as `collect` can alter this value.
        incorporatedAfter = size
        state.mkSet
    }
    final private [errors] def collect(skipNext: Int, state: HintState): Unit = if (skipNext < incorporatedAfter) {
        // This error only needs to provide the first `skipNext` elements if we encounter it again
        incorporatedAfter = skipNext
        this match {
            case EmptyHints =>
            // Popping and replacing are both achieved by skipping the next hint to be processed
            case self: PopHints => self.hints.collect(skipNext + 1, state)
            case self: ReplaceHint =>
                // replacing a hint already skips on its own anyway, so this is like skipNext - 1 + 1 == skipNext
                if (skipNext > 0) self.hints.collect(skipNext, state)
                else {
                    state += Desc(self.label)
                    self.hints.collect(skipNext + 1, state)
                }
            case self: MergeHints =>
                // if there are less hints in the first set than we want to skip, then it can be hopped over
                if (self.oldHints.size <= skipNext) self.newHints.collect(skipNext - self.oldHints.size, state)
                else {
                    // otherwise, we know that all the hints to skip are in the first set (with some which are needed!),
                    // and we can travese the second from the beginning
                    self.oldHints.collect(skipNext, state)
                    self.newHints.collect(0, state)
                }
            case self: AddError =>
                // skipping happens inside out here: the hints structure is a snoc-list
                // so first self.hints should be processed /technically/, however that would make this non-tail recursive
                // Good news is that the order the hints are /contributed/ in doesn't matter, because it's a set
                // so the order can be reversed.
                // We incorporate this error only when self.hints absorbed all the skips (skipNext <= self.hints.size)
                if (skipNext < self.size) self.err.collectHints(state)
                self.hints.collect(skipNext, state)
        }
    }

    // Operations: these are the smart constructors for the hint operations, which will reduce the number of objects in the binary
    // they all perform some form of simplification step to avoid unnecesary allocations

    
    private [machine] final def pop: DefuncHints = if (size > 1) new PopHints(this) else EmptyHints
    private [machine] final def rename(label: String): DefuncHints = if (nonEmpty) new ReplaceHint(label, this) else this
    private [machine] final def merge(newHints: DefuncHints): DefuncHints = {
        if (this.isEmpty) newHints
        else if (newHints.isEmpty) this
        else new MergeHints(this, newHints)
    }
    private [machine] final def addError(err: DefuncError): DefuncHints = new AddError(this, err)
}

/** Represents no hints at all.
  */
private [machine] object EmptyHints extends DefuncHints(size = 0)

/** This operation is used by the `hide` combinator to remove the first
  * set of hints currently in-flight. This is explicitly following the
  * behaviour of megaparsec.
  *
  * @param hints the hints that should have an element removed
  */
private [machine] final class PopHints private [errors] (val hints: DefuncHints) extends DefuncHints(size = hints.size - 1)

/** This operation is used by the `label` combinator to rename the first
  * set of hints currently in-flight. This is explicitly following the
  * behaviour of megaparsec.
  *
  * @param label the name to replace the first set of hints with
  * @param hints the hints that should have part of it replaced
  */
private [errors] final class ReplaceHint private [errors] (val label: String, val hints: DefuncHints) extends DefuncHints(size = hints.size)

/** This operation merges two sets of hints together. This used by `label`
  * to combine the saved hints with those that may have been generated and
  * affected by the label. This is not like a set-union, however, and is
  * more like a `++` on the list of sets from each error.
  *
  * @param oldHints
  * @param newHints
  */
private [errors] final class MergeHints private [errors] (val oldHints: DefuncHints, val newHints: DefuncHints) extends DefuncHints(size = oldHints.size + newHints.size)

/** This represents the snocing of a new set of hints onto the existing list of
  * sets. This is used whenever an error message is discarded by a parser succeeding
  * again.
  *
  * @param hints the initial list of sets of error items, as represented by `DefuncHints`
  * @param err the set of error items to incorporate, represented in uncomputed form as `DefuncError`
  */
private [machine] final class AddError private [errors] (val hints: DefuncHints, val err: DefuncError) extends DefuncHints(size = hints.size + 1)

/* SPDX-FileCopyrightText: © 2021 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.errors

import parsley.XAssert._

import parsley.internal.errors.{ExpectDesc, ExpectItem}

/** This structure represents a collection of operations that can be performed
  * between `Set[ErrorItem]`, which are known as `Hints`. Each set in the
  * structure is sourced from an error message directly. The collapsing of the
  * hints structure will produce a final set of error items.
  *
  * @param size the number of hint sets represented by this structure
  */
private [machine] sealed abstract class DefuncHints {
    private [errors] def isEmpty: Boolean
    private [errors] def nonEmpty: Boolean = !isEmpty
    /** This function evaluates this `DefuncHints` structure into the actual set of
      * error items it represents and adds this directly into the provided `TrivialErrorBuilder`
      */
    private [machine] def updateExpectedsAndGetSize(builder: TrivialErrorBuilder): Option[Int] = {
        val hintCollector = builder.makeHintCollector
        collect(hintCollector)
        hintCollector.unexpectWidth
    }
    /** This function evaulates this `DefuncHints` structure into an actual set of
      * error items independently of any error messages.
      *
      * @note this function is ''pure'' and can be used at will
      */
    private [machine] def toSet: Set[ExpectItem] = {
        val state: HintCollector = new HintCollector
        collect(state)
        state.mkSet
    }
    final private [errors] def collect(collector: HintCollector): Unit = {
        this match {
            case EmptyHints =>
            case self: ReplaceHint => collector ++= self.labels.map(new ExpectDesc(_))
            case self: MergeHints =>
                self.oldHints.collect(collector)
                self.newHints.collect(collector)
            case self: AddError =>
                self.err.collectHints(collector)
                self.hints.collect(collector)
        }
    }

    // Operations: these are the smart constructors for the hint operations, which will reduce the number of objects in the binary
    // they all perform some form of simplification step to avoid unnecesary allocations

    /** This operation is used by the `hide` combinator to remove all the hints currently in-flight.
     *
     * @note this behaviour was altered in 4.0.0, to match the changes in [[https://github.com/mrkkrp/megaparsec/issues/482 mrkkrp/megaparsec#482]]
     */
    private [machine] final def pop: DefuncHints = EmptyHints
    /** This operation is used by the `label` combinator to replace the
      * set of hints currently in-flight.
      *
      * @param label the name to replace the first set of hints with
      * @note this behaviour was altered in 4.0.0, to match the changes in [[https://github.com/mrkkrp/megaparsec/issues/482 mrkkrp/megaparsec#482]]
      */
    private [machine] final def rename(labels: Iterable[String]): DefuncHints = if (nonEmpty) new ReplaceHint(labels) else this
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
private [machine] object EmptyHints extends DefuncHints {
    override private [errors] def isEmpty: Boolean = true
}
private [errors] final class ReplaceHint private [errors] (val labels: Iterable[String]) extends DefuncHints {
    assume(labels.nonEmpty)
    override private [errors] def isEmpty: Boolean = false
}

private [errors] final class MergeHints private [errors] (val oldHints: DefuncHints, val newHints: DefuncHints) extends DefuncHints {
    assume(oldHints.nonEmpty && newHints.nonEmpty)
    override private [errors] def isEmpty: Boolean = false
}

private [machine] final class AddError private [errors] (val hints: DefuncHints, val err: TrivialDefuncError) extends DefuncHints {
    override private [errors] def isEmpty: Boolean = false
}

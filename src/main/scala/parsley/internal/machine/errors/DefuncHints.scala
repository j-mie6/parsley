package parsley.internal.machine.errors

import parsley.internal.errors.{TrivialError, ErrorItem, Desc}

import scala.collection.mutable
import scala.annotation.tailrec

// TODO: After this system is in place, we need a similar one which tracks what indices of each object
// have made it into the final value already. If an object is encountered again (i.e. during a merge)
// we can safely discard the computation of all values which have already appeared in the output: this
// reduces the potential complexity of the evaluator down from O(2^n) down to O(n)
private [machine] sealed abstract class DefuncHints {
    private [errors] val size: Int
    private [errors] def nonEmpty: Boolean = size != 0
    private [errors] def isEmpty: Boolean = size == 0
    private [machine] def toSet(implicit builder: ErrorItemBuilder): Set[ErrorItem] = {
        val set = mutable.Set.empty[ErrorItem]
        collect(set, 0)
        set.toSet
    }
    @tailrec final private [errors] def collect(set: mutable.Set[ErrorItem], skipNext: Int)(implicit builder: ErrorItemBuilder): Unit = this match {
        case EmptyHints =>
        case self: PopHints => self.hints.collect(set, skipNext + 1)
        case self: ReplaceHint =>
            if (skipNext > 0) self.hints.collect(set, skipNext)
            else self.hints.collect(set += Desc(self.label), skipNext+1)
        case self: MergeHints =>
            if (self.oldHints.size < skipNext) self.newHints.collect(set, skipNext - self.oldHints.size)
            else {
                self.oldHints.collectNonTail(set, skipNext)
                self.newHints.collect(set, 0)
            }
        case self: AddError =>
            if (skipNext - self.hints.size <= 0) self.err.collectHints(set)
            self.hints.collect(set, skipNext)
    }
    final private def collectNonTail(set: mutable.Set[ErrorItem], skipNext: Int)(implicit builder: ErrorItemBuilder): Unit = collect(set, skipNext)
}

private [machine] case object EmptyHints extends DefuncHints {
    val size = 0
}

private [machine] case class PopHints private (hints: DefuncHints) extends DefuncHints {
    val size = hints.size - 1
}
private [machine] object PopHints {
    def apply(hints: DefuncHints): DefuncHints = if (hints.size > 1) new PopHints(hints) else EmptyHints
}

private [errors] case class ReplaceHint private (label: String, hints: DefuncHints) extends DefuncHints {
    val size = hints.size
}
private [machine] object ReplaceHint {
    def apply(label: String, hints: DefuncHints): DefuncHints = if (hints.nonEmpty) new ReplaceHint(label, hints) else hints
}

private [errors] case class MergeHints private (oldHints: DefuncHints, newHints: DefuncHints) extends DefuncHints {
    val size = oldHints.size + newHints.size
}
private [machine] object MergeHints {
    def apply(oldHints: DefuncHints, newHints: DefuncHints): DefuncHints = {
        if (oldHints.isEmpty) newHints
        else if (newHints.isEmpty) oldHints
        else new MergeHints(oldHints, newHints)
    }
}

private [machine] case class AddError(hints: DefuncHints, err: DefuncError) extends DefuncHints {
    val size = hints.size + 1
}
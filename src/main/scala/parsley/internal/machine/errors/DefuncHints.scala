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
    private [errors] def collect(set: mutable.Set[ErrorItem], skipNext: Int)(implicit builder: ErrorItemBuilder): Int
    //@tailrec final private [errors] def collect(set: mutable.Set[ErrorItem], skipNext: Int)(implicit builder: ErrorItemBuilder): Int = this match
}

private [machine] case object EmptyHints extends DefuncHints {
    val size = 0
    def collect(set: mutable.Set[ErrorItem], skipNext: Int)(implicit builder: ErrorItemBuilder): Int = skipNext
}

private [machine] case class PopHints private (hints: DefuncHints) extends DefuncHints {
    val size = hints.size - 1
    def collect(set: mutable.Set[ErrorItem], skipNext: Int)(implicit builder: ErrorItemBuilder): Int = {
        hints.collect(set, skipNext + 1)
    }
}
private [machine] object PopHints {
    def apply(hints: DefuncHints): DefuncHints = if (hints.size > 1) new PopHints(hints) else EmptyHints
}

private [errors] case class ReplaceHint private (label: String, hints: DefuncHints) extends DefuncHints {
    val size = hints.size
    def collect(set: mutable.Set[ErrorItem], skipNext: Int)(implicit builder: ErrorItemBuilder): Int = {
        if (skipNext > 0) hints.collect(set, skipNext)
        else hints.collect(set += Desc(label), skipNext+1)
    }
}
private [machine] object ReplaceHint {
    def apply(label: String, hints: DefuncHints): DefuncHints = if (hints.nonEmpty) new ReplaceHint(label, hints) else hints
}

private [errors] case class MergeHints private (oldHints: DefuncHints, newHints: DefuncHints) extends DefuncHints {
    val size = oldHints.size + newHints.size
    def collect(set: mutable.Set[ErrorItem], skipNext: Int)(implicit builder: ErrorItemBuilder): Int = {
        val skipNext_ =
            if (oldHints.size < skipNext) skipNext - oldHints.size
            else oldHints.collect(set, skipNext)
        newHints.collect(set, skipNext_)
    }
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
    def collect(set: mutable.Set[ErrorItem], skipNext: Int)(implicit builder: ErrorItemBuilder): Int = {
        hints.collect(set, skipNext) match {
            case 0 =>
                err.collectHints(set)
                0
            case skipNext => skipNext - 1
        }
    }
}
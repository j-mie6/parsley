package parsley.internal.machine.errors

import parsley.internal.errors.{TrivialError, ErrorItem, Desc}

import scala.collection.mutable

// TODO: We can optimise the way this works by adding "do-not-compute" indices during evaluation
// This means that merging hints does not need to make a new buffer and unneeded work can be skipped
// entirely.
// TODO: After this system is in place, we need a similar one which tracks what indices of each object
// have made it into the final value already. If an object is encountered again (i.e. during a merge)
// we can safely discard the computation of all values which have already appeared in the output: this
// reduces the potential complexity of the evaluator down from O(2^n) down to O(n)
private [machine] sealed abstract class DefuncHints {
    private [errors] val size: Int
    private [errors] def nonEmpty: Boolean = size != 0
    private [errors] def isEmpty: Boolean = size == 0
    private [machine] def toSet(implicit builder: ErrorItemBuilder): Set[ErrorItem] = {
        toList.foldLeft(Set.empty[ErrorItem])(_ union _)
    }
    private [errors] def toList(implicit builder: ErrorItemBuilder): List[Set[ErrorItem]] = {
        val buff = mutable.ListBuffer.empty[Set[ErrorItem]]
        val skipNext = collect(buff, 0)
        assert(skipNext == 0)
        buff.toList
    }
    private [errors] def collect(buff: mutable.ListBuffer[Set[ErrorItem]], skipNext: Int)(implicit builder: ErrorItemBuilder): Int
}

private [machine] case object EmptyHints extends DefuncHints {
    val size = 0
    def collect(buff: mutable.ListBuffer[Set[ErrorItem]], skipNext: Int)(implicit builder: ErrorItemBuilder): Int = skipNext
}

private [machine] case class PopHints private (hints: DefuncHints) extends DefuncHints {
    val size = hints.size - 1
    def collect(buff: mutable.ListBuffer[Set[ErrorItem]], skipNext: Int)(implicit builder: ErrorItemBuilder): Int = {
        hints.collect(buff, skipNext + 1)
    }
}
private [machine] object PopHints {
    def apply(hints: DefuncHints): DefuncHints = if (hints.size > 1) new PopHints(hints) else EmptyHints
}

private [errors] case class ReplaceHint private (label: String, hints: DefuncHints) extends DefuncHints {
    val size = hints.size
    def collect(buff: mutable.ListBuffer[Set[ErrorItem]], skipNext: Int)(implicit builder: ErrorItemBuilder): Int = {
        if (skipNext > 0) hints.collect(buff, skipNext)
        else {
            buff += Set(Desc(label))
            hints.collect(buff, skipNext+1)
        }
    }
}
private [machine] object ReplaceHint {
    def apply(label: String, hints: DefuncHints): DefuncHints = if (hints.nonEmpty) new ReplaceHint(label, hints) else hints
}

private [errors] case class MergeHints private (oldHints: DefuncHints, newHints: DefuncHints) extends DefuncHints {
    val size = oldHints.size + newHints.size
    def collect(buff: mutable.ListBuffer[Set[ErrorItem]], skipNext: Int)(implicit builder: ErrorItemBuilder): Int = {
        val skipNext_ = oldHints.collect(buff, skipNext)
        val newBuff = mutable.ListBuffer.empty[Set[ErrorItem]]
        val skipNext__ = newHints.collect(newBuff, skipNext_)
        buff ++= newBuff.toList
        skipNext__
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
    def collect(buff: mutable.ListBuffer[Set[ErrorItem]], skipNext: Int)(implicit builder: ErrorItemBuilder): Int = {
        val skipNext_ = hints.collect(buff, skipNext)
        if (skipNext_ == 0) {
            val TrivialError(_, _, _, _, es, _) = err.asParseError
            buff += es
            0
        }
        else skipNext_ - 1
    }
}
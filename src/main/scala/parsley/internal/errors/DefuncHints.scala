package parsley.internal.errors

import scala.collection.mutable

// TODO: We can optimise the way this works by adding "do-not-compute" indices during evaluation
// This means that merging hints does not need to make a new buffer and unneeded work can be skipped
// entirely.
// TODO: After this system is in place, we need a similar one which tracks what indices of each object
// have made it into the final value already. If an object is encountered again (i.e. during a merge)
// we can safely discard the computation of all values which have already appeared in the output: this
// reduces the potential complexity of the evaluator down from O(2^n) down to O(n)
private [internal] sealed abstract class DefuncHints {
    private [errors] val size: Int
    private [errors] def nonEmpty: Boolean = size != 0
    private [errors] def isEmpty: Boolean = size == 0
    private [internal] def toList(implicit builder: ErrorItemBuilder): List[Set[ErrorItem]] = {
        val buff = mutable.ListBuffer.empty[Set[ErrorItem]]
        collect(buff)
        buff.toList
    }
    private [errors] def collect(buff: mutable.ListBuffer[Set[ErrorItem]])(implicit builder: ErrorItemBuilder): Unit
}

private [internal] case object EmptyHints extends DefuncHints {
    val size = 0
    def collect(buff: mutable.ListBuffer[Set[ErrorItem]])(implicit builder: ErrorItemBuilder): Unit = ()
}

private [internal] case class PopHints private (hints: DefuncHints) extends DefuncHints {
    val size = hints.size - 1
    def collect(buff: mutable.ListBuffer[Set[ErrorItem]])(implicit builder: ErrorItemBuilder): Unit = {
        hints.collect(buff)
        buff.remove(0)
    }
}
private [internal] object PopHints {
    def apply(hints: DefuncHints): DefuncHints = if (hints.size > 1) new PopHints(hints) else EmptyHints
}

private [errors] case class ReplaceHint private (label: String, hints: DefuncHints) extends DefuncHints {
    val size = hints.size
    def collect(buff: mutable.ListBuffer[Set[ErrorItem]])(implicit builder: ErrorItemBuilder): Unit = {
        hints.collect(buff)
        buff(0) = Set(Desc(label))
    }
}
private [internal] object ReplaceHint {
    def apply(label: String, hints: DefuncHints): DefuncHints = if (hints.nonEmpty) new ReplaceHint(label, hints) else hints
}

private [errors] case class MergeHints private (oldHints: DefuncHints, newHints: DefuncHints) extends DefuncHints {
    val size = oldHints.size + newHints.size
    def collect(buff: mutable.ListBuffer[Set[ErrorItem]])(implicit builder: ErrorItemBuilder): Unit = {
        oldHints.collect(buff)
        buff ++= newHints.toList
    }
}
private [internal] object MergeHints {
    def apply(oldHints: DefuncHints, newHints: DefuncHints): DefuncHints = {
        if (oldHints.isEmpty) newHints
        else if (newHints.isEmpty) oldHints
        else new MergeHints(oldHints, newHints)
    }
}

private [internal] case class AddError(hints: DefuncHints, err: DefuncError) extends DefuncHints {
    val size = hints.size + 1
    def collect(buff: mutable.ListBuffer[Set[ErrorItem]])(implicit builder: ErrorItemBuilder): Unit = {
        hints.collect(buff)
        val TrivialError(_, _, _, _, es, _) = err.asParseError
        buff += es
    }
}
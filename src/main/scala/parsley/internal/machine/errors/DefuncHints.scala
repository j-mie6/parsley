package parsley.internal.machine.errors

import parsley.internal.errors.{ErrorItem, Desc}

import scala.collection.mutable
import scala.annotation.tailrec

private [machine] sealed abstract class DefuncHints(private [errors] val size: Int) {
    private var incorporatedAfter: Int = size
    private [errors] def nonEmpty: Boolean = size != 0
    private [errors] def isEmpty: Boolean = size == 0
    private [machine] def toExpectedsAndSize: (Set[ErrorItem], Int) = {
        implicit val state: HintState = new HintState
        collect(0)
        (state.mkSet, state.unexpectSize)
    }
    private [machine] def toSet: Set[ErrorItem] = toExpectedsAndSize._1
    @tailrec final private [errors] def collect(skipNext: Int)(implicit state: HintState): Unit = if (skipNext < incorporatedAfter) {
        // This error only needs to provide the first `skipNext` elements if we encounter it again
        incorporatedAfter = skipNext
        this match {
            case EmptyHints =>
            case self: PopHints => self.hints.collect(skipNext + 1)
            case self: ReplaceHint =>
                if (skipNext > 0) self.hints.collect(skipNext)
                else {
                    state += Desc(self.label)
                    self.hints.collect(skipNext+1)
                }
            case self: MergeHints =>
                if (self.oldHints.size < skipNext) self.newHints.collect(skipNext - self.oldHints.size)
                else {
                    self.oldHints.collectNonTail(skipNext)
                    self.newHints.collect(0)
                }
            case self: AddError =>
                if (skipNext - self.hints.size <= 0) self.err.collectHints()
                self.hints.collect(skipNext)
        }
    }
    final private def collectNonTail(skipNext: Int)(implicit state: HintState): Unit = collect(skipNext)
}

private [machine] case object EmptyHints extends DefuncHints(size = 0)

private [machine] case class PopHints private (hints: DefuncHints) extends DefuncHints(size = hints.size - 1)
private [machine] object PopHints {
    def apply(hints: DefuncHints): DefuncHints = if (hints.size > 1) new PopHints(hints) else EmptyHints
}

private [errors] case class ReplaceHint private (label: String, hints: DefuncHints) extends DefuncHints(size = hints.size)
private [machine] object ReplaceHint {
    def apply(label: String, hints: DefuncHints): DefuncHints = if (hints.nonEmpty) new ReplaceHint(label, hints) else hints
}

private [errors] case class MergeHints private (oldHints: DefuncHints, newHints: DefuncHints) extends DefuncHints(size = oldHints.size + newHints.size)
private [machine] object MergeHints {
    def apply(oldHints: DefuncHints, newHints: DefuncHints): DefuncHints = {
        if (oldHints.isEmpty) newHints
        else if (newHints.isEmpty) oldHints
        else new MergeHints(oldHints, newHints)
    }
}

private [machine] case class AddError(hints: DefuncHints, err: DefuncError) extends DefuncHints(size = hints.size + 1)
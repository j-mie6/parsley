package parsley.internal.errors

private [parsley] abstract class LineBuilder {
    final def getLineWithCaret(offset: Int): (String, Int) = {
        // FIXME: Tabs man... tabs
        val startOffset = nearestNewlineBefore(offset)
        val endOffset = nearestNewlineAfter(offset)
        val segment = segmentBetween(startOffset, endOffset)
        val caretAt = offset - startOffset
        (segment.replace('\t', ' '), caretAt)
    }

    protected def nearestNewlineBefore(off: Int): Int
    protected def nearestNewlineAfter(off: Int): Int
    protected def segmentBetween(start: Int, end: Int): String
}
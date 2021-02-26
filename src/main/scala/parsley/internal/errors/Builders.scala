package parsley.internal.errors

private [internal] abstract class LineBuilder {
    final private [errors] def getLineWithCaret(offset: Int): (String, String) = {
        // FIXME: Tabs man... tabs
        val startOffset = nearestNewlineBefore(offset)
        val endOffset = nearestNewlineAfter(offset)
        val segment = segmentBetween(startOffset, endOffset)
        val caretAt = offset - startOffset
        val caretPad = " " * caretAt
        (segment.replace('\t', ' '), s"$caretPad^")
    }

    protected def nearestNewlineBefore(off: Int): Int
    protected def nearestNewlineAfter(off: Int): Int
    protected def segmentBetween(start: Int, end: Int): String
}

private [internal] abstract class ErrorItemBuilder {
    final private [errors] def apply(offset: Int): ErrorItem = {
        if (inRange(offset)) Raw(charAt(offset))
        else EndOfInput
    }
    final private [errors] def apply(offset: Int, size: Int): ErrorItem = {
        if (inRange(offset)) Raw(substring(offset, size))
        else EndOfInput
    }

    protected def inRange(offset: Int): Boolean
    protected def charAt(offset: Int): Char
    protected def substring(offset: Int, size: Int): String
}
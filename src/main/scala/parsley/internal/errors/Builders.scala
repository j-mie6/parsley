package parsley.internal.errors

import scala.annotation.tailrec
import scala.collection.mutable

private [parsley] abstract class LineBuilder {
    final def getLineWithCaret(offset: Int): Option[(String, Int)] = for {
        (line, startOffset, _) <- getLineWithEndPoints(offset)
        caretAt = offset - startOffset
    } yield (line.replace('\t', ' '), caretAt)

    final private def getLineWithEndPoints(offset: Int): Option[(String, Int, Int)] = for {
        // FIXME: Tabs man... tabs
        startOffset <- nearestNewlineBefore(offset)
        endOffset <- nearestNewlineAfter(offset)
        segment = segmentBetween(startOffset, endOffset)
        caretAt = offset - startOffset
    } yield (segment.replace('\t', ' '), startOffset, endOffset)

    @tailrec private final def getLinesBefore(offset: Int, i: Int, lines: mutable.ListBuffer[String]): Unit = if (i >= 0) getLineWithEndPoints(offset) match {
        case Some((line, startOffset, _)) =>
            line +=: lines
            getLinesBefore(startOffset-1, i - 1, lines)
        case None =>
    }

    final def getLinesBefore(offset: Int, n: Int): List[String] = {
        val lines = mutable.ListBuffer.empty[String]
        for (startOffset <- nearestNewlineBefore(offset)) getLinesBefore(startOffset-1, n-1, lines)
        lines.toList
    }

    private final def getLinesAfter(offset: Int, i: Int, lines: mutable.ListBuffer[String]): Unit = if (i >= 0) getLineWithEndPoints(offset) match {
        case Some((line, _, endOffset)) =>
            lines += line
            getLinesAfter(endOffset+1, i - 1, lines)
        case None =>
    }

    final def getLinesAfter(offset: Int, n: Int): List[String] = {
        val lines = mutable.ListBuffer.empty[String]
        for (endOffset <- nearestNewlineAfter(offset)) getLinesAfter(endOffset+1, n-1, lines)
        lines.toList
    }

    protected def nearestNewlineBefore(off: Int): Option[Int]
    protected def nearestNewlineAfter(off: Int): Option[Int]
    protected def segmentBetween(start: Int, end: Int): String
}
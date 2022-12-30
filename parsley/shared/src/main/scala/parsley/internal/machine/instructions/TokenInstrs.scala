/* SPDX-FileCopyrightText: © 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.instructions

import scala.annotation.tailrec

import parsley.XAssert._
import parsley.token.descriptions.SpaceDesc
import parsley.token.errors.ErrorConfig

import parsley.internal.errors.{ExpectDesc, ExpectItem, UnexpectDesc}
import parsley.internal.machine.Context
import parsley.internal.machine.XAssert._

private [instructions] abstract class CommentLexer extends Instr {
    protected [this] val start: String
    protected [this] val end: String
    protected [this] val line: String
    protected [this] val nested: Boolean
    protected [this] val eofAllowed: Boolean
    protected [this] final val lineAllowed = line.nonEmpty
    protected [this] final val multiAllowed = start.nonEmpty && end.nonEmpty

    assert(!lineAllowed || !multiAllowed || !line.startsWith(start), "multi-line comments may not prefix single-line comments")

    protected final def singleLineComment(ctx: Context): Boolean = {
        ctx.fastUncheckedConsumeChars(line.length)
        consumeSingle(ctx)
    }

    @tailrec private final def consumeSingle(ctx: Context): Boolean = {
        if (ctx.moreInput) {
            if (ctx.nextChar != '\n') {
                ctx.consumeChar()
                consumeSingle(ctx)
            }
            else true
        }
        else eofAllowed
    }

    @tailrec private final def wellNested(ctx: Context, unmatched: Int): Boolean = {
        if (unmatched == 0) true
        else if (ctx.input.startsWith(end, ctx.offset)) {
            ctx.fastUncheckedConsumeChars(end.length)
            wellNested(ctx, unmatched - 1)
        }
        else if (nested && ctx.input.startsWith(start, ctx.offset)) {
            ctx.fastUncheckedConsumeChars(start.length)
            wellNested(ctx, unmatched + 1)
        }
        else if (ctx.moreInput) {
            ctx.consumeChar()
            wellNested(ctx, unmatched)
        }
        else false
    }

    protected final def multiLineComment(ctx: Context): Boolean = {
        ctx.fastUncheckedConsumeChars(start.length)
        wellNested(ctx, 1)
    }
}

private [internal] final class TokenComment private (
    protected [this] val start: String,
    protected [this] val end: String,
    protected [this] val line: String,
    protected [this] val nested: Boolean,
    protected [this] val eofAllowed: Boolean,
    endOfMultiComment: Option[ExpectItem],
    endOfSingleComment: Option[ExpectDesc],
    ) extends CommentLexer {
    def this(desc: SpaceDesc, errConfig: ErrorConfig) = {
        this(desc.commentStart, desc.commentEnd, desc.commentLine, desc.nestedComments, desc.commentLineAllowsEOF,
             ExpectItem(errConfig.labelSpaceEndOfMultiComment, desc.commentEnd),
             ExpectDesc(errConfig.labelSpaceEndOfLineComment, "end of line"))
    }
    private [this] final val openingSize = Math.max(start.codePointCount(0, start.length), line.codePointCount(0, line.length))

    assert(multiAllowed || lineAllowed, "one of single- or multi-line must be enabled")

    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        val startsMulti = multiAllowed && ctx.input.startsWith(start, ctx.offset)
        // If neither comment is available we fail
        if (!ctx.moreInput || (!lineAllowed || !ctx.input.startsWith(line, ctx.offset)) && !startsMulti) ctx.expectedFail(expected = None, openingSize)
        // One of the comments must be available
        else if (startsMulti && multiLineComment(ctx)) ctx.pushAndContinue(())
        else if (startsMulti) ctx.expectedFail(expected = endOfMultiComment, unexpectedWidth = 1)
        // It clearly wasn't the multi-line comment, so we are left with single line
        else if (singleLineComment(ctx)) ctx.pushAndContinue(())
        else ctx.expectedFail(expected = endOfSingleComment, unexpectedWidth = 1)
    }

    // $COVERAGE-OFF$
    override def toString: String = "TokenComment"
    // $COVERAGE-ON$
}

private [instructions] abstract class WhiteSpaceLike extends CommentLexer {
    private [this] final val numCodePointsEnd = end.codePointCount(0, end.length)
    protected [this] val endOfSingleComment: Option[ExpectDesc]// = Some(ExpectDesc("end of comment")) //FIXME:
    protected [this] val endOfMultiComment: Option[ExpectItem]// = Some(ExpectDesc("end of comment")) //FIXME:
    @tailrec private final def singlesOnly(ctx: Context): Unit = {
        spaces(ctx)
        if (ctx.moreInput) {
            val startsSingle = ctx.input.startsWith(line, ctx.offset)
            if (startsSingle && singleLineComment(ctx)) singlesOnly(ctx)
            else if (startsSingle) ctx.expectedFail(expected = endOfSingleComment, unexpectedWidth = 1)
            else ctx.pushAndContinue(())
        }
        else ctx.pushAndContinue(())
    }

    @tailrec private final def multisOnly(ctx: Context): Unit = {
        spaces(ctx)
        val startsMulti = ctx.moreInput && ctx.input.startsWith(start, ctx.offset)
        if (startsMulti && multiLineComment(ctx)) multisOnly(ctx)
        else if (startsMulti) ctx.expectedFail(expected = endOfMultiComment, numCodePointsEnd)
        else ctx.pushAndContinue(())
    }

    private [this] final val sharedPrefix = line.view.zip(start).takeWhile(Function.tupled(_ == _)).map(_._1).mkString
    private [this] final val factoredStart = start.drop(sharedPrefix.length)
    private [this] final val factoredLine = line.drop(sharedPrefix.length)
    // PRE: Multi-line comments may not prefix single-line, but single-line may prefix multi-line
    @tailrec final def singlesAndMultis(ctx: Context): Unit = {
        spaces(ctx)
        if (ctx.moreInput && ctx.input.startsWith(sharedPrefix, ctx.offset)) {
            val startsMulti = ctx.input.startsWith(factoredStart, ctx.offset + sharedPrefix.length)
            if (startsMulti && multiLineComment(ctx)) singlesAndMultis(ctx)
            else if (startsMulti) ctx.expectedFail(expected = endOfMultiComment, numCodePointsEnd)
            else {
                val startsLine = ctx.input.startsWith(factoredLine, ctx.offset + sharedPrefix.length)
                if (startsLine && singleLineComment(ctx)) singlesAndMultis(ctx)
                else if (startsLine) ctx.expectedFail(expected = endOfSingleComment, unexpectedWidth = 1)
                else ctx.pushAndContinue(())
            }
        }
        else ctx.pushAndContinue(())
    }

    final def spacesAndContinue(ctx: Context): Unit = {
        spaces(ctx)
        ctx.pushAndContinue(())
    }

    private [this] final val impl = {
        if (!lineAllowed && !multiAllowed) spacesAndContinue(_)
        else if (!lineAllowed) multisOnly(_)
        else if (!multiAllowed) singlesOnly(_)
        else singlesAndMultis(_)
    }

    override final def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        impl(ctx)
    }
    protected def spaces(ctx: Context): Unit
}

private [internal] final class TokenWhiteSpace private (
    ws: Char => Boolean,
    protected [this] val start: String,
    protected [this] val end: String,
    protected [this] val line: String,
    protected [this] val nested: Boolean,
    protected [this] val eofAllowed: Boolean,
    protected [this] val endOfMultiComment: Option[ExpectItem],
    protected [this] val endOfSingleComment: Option[ExpectDesc]) extends WhiteSpaceLike {
    def this(ws: Char => Boolean, desc: SpaceDesc, errConfig: ErrorConfig) = {
        this(ws, desc.commentStart, desc.commentEnd, desc.commentLine, desc.nestedComments, desc.commentLineAllowsEOF,
             ExpectItem(errConfig.labelSpaceEndOfMultiComment, desc.commentEnd),
             ExpectDesc(errConfig.labelSpaceEndOfLineComment, "end of line"))
    }
    override def spaces(ctx: Context): Unit = {
        while (ctx.moreInput && ws(ctx.nextChar)) {
            ctx.consumeChar()
        }
    }
    // $COVERAGE-OFF$
    override def toString: String = "TokenWhiteSpace"
    // $COVERAGE-ON$
}

private [internal] final class TokenSkipComments private (
    protected [this] val start: String,
    protected [this] val end: String,
    protected [this] val line: String,
    protected [this] val nested: Boolean,
    protected [this] val eofAllowed: Boolean,
    protected [this] val endOfMultiComment: Option[ExpectItem],
    protected [this] val endOfSingleComment: Option[ExpectDesc]) extends WhiteSpaceLike {
    def this(desc: SpaceDesc, errConfig: ErrorConfig) = {
        this(desc.commentStart, desc.commentEnd, desc.commentLine, desc.nestedComments, desc.commentLineAllowsEOF,
             ExpectItem(errConfig.labelSpaceEndOfMultiComment, desc.commentEnd),
             ExpectDesc(errConfig.labelSpaceEndOfLineComment, "end of line"))
    }
    override def spaces(ctx: Context): Unit = ()
    // $COVERAGE-OFF$
    override def toString: String = "TokenSkipComments"
    // $COVERAGE-ON$
}

private [internal] final class TokenNonSpecific(name: String, unexpectedIllegal: String => String)
                                               (start: Char => Boolean, letter: Char => Boolean, illegal: String => Boolean) extends Instr {
    private [this] final val expected = Some(new ExpectDesc(name))

    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        if (ctx.moreInput && start(ctx.nextChar)) {
            val initialOffset = ctx.offset
            ctx.offset += 1
            restOfToken(ctx, initialOffset)
        }
        else ctx.expectedFail(expected, unexpectedWidth = 1)
    }

    private def ensureLegal(ctx: Context, tok: String) = {
        if (illegal(tok)) {
            ctx.offset -= tok.length
            ctx.unexpectedFail(expected = expected, unexpected = new UnexpectDesc(unexpectedIllegal(tok), tok.length))
        }
        else {
            ctx.col += tok.length
            ctx.pushAndContinue(tok)
        }
    }

    @tailrec private def restOfToken(ctx: Context, initialOffset: Int): Unit = {
        if (ctx.moreInput && letter(ctx.nextChar)) {
            ctx.offset += 1
            restOfToken(ctx, initialOffset)
        }
        else ensureLegal(ctx, ctx.input.substring(initialOffset, ctx.offset))
    }

    // $COVERAGE-OFF$
    override def toString: String = s"TokenNonSpecific($name)"
    // $COVERAGE-ON$
}

private [instructions] abstract class TokenSpecificAllowTrailing(
        specific: String, expected: Option[ExpectDesc], protected final val expectedEnd: Option[ExpectDesc], caseSensitive: Boolean) extends Instr {
    def this(specific: String, expected: Option[String], expectedEnd: String, caseSensitive: Boolean) = {
        this(if (caseSensitive) specific else specific.toLowerCase, ExpectDesc(expected), Some(new ExpectDesc(expectedEnd)), caseSensitive)
    }
    private [this] final val strsz = specific.length
    private [this] final val numCodePoints = specific.codePointCount(0, strsz)
    protected def postprocess(ctx: Context, i: Int): Unit

    val readCharCaseHandled = {
        if (caseSensitive) (ctx: Context, i: Int) => ctx.input.charAt(i)
        else (ctx: Context, i: Int) => ctx.input.charAt(i).toLower
    }

    @tailrec final private def readSpecific(ctx: Context, i: Int, j: Int): Unit = {
        if (j < strsz && readCharCaseHandled(ctx, i) == specific.charAt(j)) readSpecific(ctx, i + 1, j + 1)
        else if (j < strsz) ctx.expectedFail(expected, numCodePoints)
        else {
            ctx.saveState()
            ctx.fastUncheckedConsumeChars(strsz)
            postprocess(ctx, i)
        }
    }

    final override def apply(ctx: Context): Unit = {
        if (ctx.inputsz >= ctx.offset + strsz) readSpecific(ctx, ctx.offset, 0)
        else ctx.expectedFail(expected, numCodePoints)
    }
}

private [internal] final class TokenSpecific(specific: String, expected: Option[String], _expectedEnd: String, letter: Char => Boolean, caseSensitive: Boolean)
    extends TokenSpecificAllowTrailing(specific, expected, _expectedEnd, caseSensitive) {
    override def postprocess(ctx: Context, i: Int): Unit = {
        if (i < ctx.inputsz && letter(ctx.input.charAt(i))) {
            ctx.expectedFail(expectedEnd, unexpectedWidth = 1) //This should only report a single token
            ctx.restoreState()
        }
        else {
            ctx.states = ctx.states.tail
            ctx.pushAndContinue(())
        }
    }

    // $COVERAGE-OFF$
    override def toString: String = s"TokenSpecific($specific)"
    // $COVERAGE-ON$
}

/*
private [internal] final class TokenMaxOp(operator: String, _ops: Set[String]) extends TokenSpecificAllowTrailing(operator, true) {
    private val ops = Radix.makeSet(_ops.collect {
        case op if op.length > operator.length && op.startsWith(operator) => op.substring(operator.length)
    })

    @tailrec private def go(ctx: Context, i: Int, ops: RadixSet): Unit = {
        lazy val ops_ = ops.suffixes(ctx.input.charAt(i))
        val possibleOpsRemain = i < ctx.inputsz && ops.nonEmpty
        if (possibleOpsRemain && ops_.contains("")) {
            ctx.expectedFail(expectedEnd) //This should only report a single token
            ctx.restoreState()
        }
        else if (possibleOpsRemain) go(ctx, i + 1, ops_)
        else {
            ctx.states = ctx.states.tail
            ctx.pushAndContinue(())
        }
    }

    override def postprocess(ctx: Context, i: Int): Unit = go(ctx, i, ops)

    // $COVERAGE-OFF$
    override def toString: String = s"TokenMaxOp(${operator})"
    // $COVERAGE-ON$
}
*/

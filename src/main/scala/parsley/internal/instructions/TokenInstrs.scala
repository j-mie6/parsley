package parsley.internal.instructions

import parsley.internal.deepembedding.Sign.{SignType, IntType, DoubleType}
import parsley.token.TokenSet
import parsley.internal.Radix
import parsley.internal.errors.{ErrorItem, Desc}

import scala.annotation.tailrec

private [instructions] abstract class CommentLexer(start: String, end: String, line: String, nested: Boolean) extends Instr {
    protected final val lineAllowed = line.nonEmpty
    protected final val multiAllowed = start.nonEmpty && end.nonEmpty
    protected final val endOfComment = Some(Desc("end of comment"))

    protected final def singleLineComment(ctx: Context): Unit = {
        ctx.fastUncheckedConsumeChars(line.length)
        while (ctx.moreInput && ctx.nextChar != '\n') ctx.consumeChar()
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

private [instructions] abstract class WhiteSpaceLike(start: String, end: String, line: String, nested: Boolean) extends CommentLexer(start, end, line, nested) {
    @tailrec private final def singlesOnly(ctx: Context): Unit = {
        spaces(ctx)
        if (ctx.moreInput && ctx.input.startsWith(line, ctx.offset)) {
            singleLineComment(ctx)
            singlesOnly(ctx)
        }
        else ctx.pushAndContinue(())
    }

    @tailrec private final def multisOnly(ctx: Context): Unit = {
        spaces(ctx)
        val startsMulti = ctx.moreInput && ctx.input.startsWith(start, ctx.offset)
        if (startsMulti && multiLineComment(ctx)) multisOnly(ctx)
        else if (startsMulti) ctx.expectedFail(expected = endOfComment)
        else ctx.pushAndContinue(())
    }

    private val sharedPrefix = line.view.zip(start).takeWhile(Function.tupled(_ == _)).map(_._1).mkString
    private val factoredStart = start.drop(sharedPrefix.length)
    private val factoredLine = line.drop(sharedPrefix.length)
    // PRE: Multi-line comments may not prefix single-line, but single-line may prefix multi-line
    @tailrec final def singlesAndMultis(ctx: Context): Unit = {
        spaces(ctx)
        if (ctx.moreInput && ctx.input.startsWith(sharedPrefix, ctx.offset)) {
            val startsMulti = ctx.input.startsWith(factoredStart, ctx.offset + sharedPrefix.length)
            if (startsMulti && multiLineComment(ctx)) singlesAndMultis(ctx)
            else if (startsMulti) ctx.expectedFail(expected = endOfComment)
            else if (ctx.input.startsWith(factoredLine, ctx.offset + sharedPrefix.length)) {
                singleLineComment(ctx)
                singlesAndMultis(ctx)
            }
        }
        else ctx.pushAndContinue(())
    }

    private final val impl = {
        if (!lineAllowed) multisOnly(_)
        else if (!multiAllowed) singlesOnly(_)
        else singlesAndMultis(_)
    }

    override final def apply(ctx: Context): Unit = impl(ctx)
    protected def spaces(ctx: Context): Unit
}

private [internal] final class TokenComment(start: String, end: String, line: String, nested: Boolean) extends CommentLexer(start, end, line, nested) {
    private [this] final val comment = Some(Desc("comment"))

    // PRE: one of the comments is supported
    // PRE: Multi-line comments may not prefix single-line, but single-line may prefix multi-line
    override def apply(ctx: Context): Unit = {
        val startsMulti = multiAllowed && ctx.input.startsWith(start, ctx.offset)
        // If neither comment is available we fail
        if (!ctx.moreInput || (!lineAllowed || !ctx.input.startsWith(line, ctx.offset)) && !startsMulti) ctx.expectedFail(expected = comment)
        // One of the comments must be available
        else if (startsMulti && multiLineComment(ctx)) ctx.pushAndContinue(())
        else if (startsMulti) ctx.expectedFail(expected = endOfComment)
        // It clearly wasn't the multi-line comment, so we are left with single line
        else {
            singleLineComment(ctx)
            ctx.pushAndContinue(())
        }
    }

    // $COVERAGE-OFF$
    override def toString: String = "TokenComment"
    // $COVERAGE-ON$
}

private [internal] final class TokenWhiteSpace(ws: TokenSet, start: String, end: String, line: String, nested: Boolean)
    extends WhiteSpaceLike(start, end, line, nested) {
    override def spaces(ctx: Context): Unit = while (ctx.moreInput && ws(ctx.nextChar)) ctx.consumeChar()
    // $COVERAGE-OFF$
    override def toString: String = "TokenWhiteSpace"
    // $COVERAGE-ON$
}

private [internal] final class TokenSkipComments(start: String, end: String, line: String, nested: Boolean) extends WhiteSpaceLike(start, end, line, nested) {
    override def spaces(ctx: Context): Unit = ()
    // $COVERAGE-OFF$
    override def toString: String = "TokenSkipComments"
    // $COVERAGE-ON$
}

private [internal] final class TokenNonSpecific(name: String, illegalName: String)
                                               (start: TokenSet, letter: TokenSet, illegal: String => Boolean, _expected: Option[String]) extends Instr {
    //val expected: Some = Set(Desc(_expected.getOrElse(name)))
    private [this] final val expected = Some(Desc(_expected.getOrElse(name)))

    override def apply(ctx: Context): Unit = {
        if (ctx.moreInput && start(ctx.nextChar)) {
            val initialOffset = ctx.offset
            ctx.offset += 1
            restOfToken(ctx, initialOffset)
        }
        else ctx.expectedFail(expected)
    }

    private def ensureLegal(ctx: Context, tok: String) = {
        if (illegal(tok)) {
            ctx.offset -= tok.length
            ctx.unexpectedFail(expected = expected, unexpected = new Desc(s"$illegalName $tok"))
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

private [instructions] abstract class TokenSpecificAllowTrailing(_specific: String, caseSensitive: Boolean, _expected: Option[String]) extends Instr {
    private final val expected = Some(Desc(_expected.getOrElse(_specific)))
    protected final val expectedEnd = Some(Desc(_expected.getOrElse(s"end of ${_specific}")))
    private final val specific = (if (caseSensitive) _specific else _specific.toLowerCase).toCharArray
    private final val strsz = specific.length
    protected def postprocess(ctx: Context, i: Int): Unit

    val readCharCaseHandled = {
        if (caseSensitive) (ctx: Context, i: Int) => ctx.input.charAt(i)
        else (ctx: Context, i: Int) => ctx.input.charAt(i).toLower
    }

    @tailrec final private def readSpecific(ctx: Context, i: Int, j: Int): Unit = {
        if (j < strsz && readCharCaseHandled(ctx, i) == specific(j)) readSpecific(ctx, i + 1, j + 1)
        else if (j < strsz) ctx.expectedFail(expected)
        else {
            ctx.saveState()
            ctx.fastUncheckedConsumeChars(strsz)
            postprocess(ctx, i)
        }
    }

    final override def apply(ctx: Context): Unit = {
        if (ctx.inputsz >= ctx.offset + strsz) readSpecific(ctx, ctx.offset, 0)
        else ctx.expectedFail(expected)
    }
}

private [internal] final class TokenSpecific(_specific: String, letter: TokenSet, caseSensitive: Boolean, expected: Option[String])
    extends TokenSpecificAllowTrailing(_specific, caseSensitive, expected) {
    override def postprocess(ctx: Context, i: Int): Unit = {
        if (i < ctx.inputsz && letter(ctx.input.charAt(i))) {
            ctx.expectedFail(expectedEnd)
            ctx.restoreState()
        }
        else {
            ctx.states = ctx.states.tail
            ctx.pushAndContinue(())
        }
    }

    // $COVERAGE-OFF$
    override def toString: String = s"TokenSpecific(${_specific})"
    // $COVERAGE-ON$
}

private [internal] final class TokenMaxOp(operator: String, _ops: Set[String], expected: Option[String])
    extends TokenSpecificAllowTrailing(operator, true, expected) {
    private val ops = Radix(_ops.collect {
        case op if op.length > operator.length && op.startsWith(operator) => op.substring(operator.length)
    })

    @tailrec private def go(ctx: Context, i: Int, ops: Radix[Unit]): Unit = {
        lazy val ops_ = ops.suffixes(ctx.input.charAt(i))
        val possibleOpsRemain = i < ctx.inputsz && ops.nonEmpty
        if (possibleOpsRemain && ops_.contains("")) {
            ctx.expectedFail(expectedEnd)
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
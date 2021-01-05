package parsley.internal.instructions

import parsley.internal.deepembedding.Sign.{SignType, IntType, DoubleType}
import parsley.TokenParser.TokenSet
import parsley.internal.{Radix, UnsafeOption}

import scala.annotation.tailrec

private [instructions] abstract class CommentLexer(start: String, end: String, line: String, nested: Boolean) extends Instr {
    protected final val noLine = line.isEmpty
    protected final val noMulti = start.isEmpty || end.isEmpty

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
        else if (startsMulti) ctx.fail("end of comment")
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
            else if (startsMulti) ctx.fail("end of comment")
            else if (ctx.input.startsWith(factoredLine, ctx.offset + sharedPrefix.length)) {
                singleLineComment(ctx)
                singlesAndMultis(ctx)
            }
        }
        else ctx.pushAndContinue(())
    }

    private final val impl = {
        if (noLine) multisOnly(_)
        else if (noMulti) singlesOnly(_)
        else singlesAndMultis(_)
    }

    override final def apply(ctx: Context): Unit = impl(ctx)
    protected def spaces(ctx: Context): Unit
}

private [internal] final class TokenComment(start: String, end: String, line: String, nested: Boolean) extends CommentLexer(start, end, line, nested) {
    // PRE: one of the comments is supported
    // PRE: Multi-line comments may not prefix single-line, but single-line may prefix multi-line
    override def apply(ctx: Context): Unit = {
        val startsMulti = !noMulti && ctx.input.startsWith(start, ctx.offset)
        // If neither comment is available we fail
        if (!ctx.moreInput || (!noLine && !ctx.input.startsWith(line, ctx.offset)) && (!noMulti && !startsMulti)) ctx.fail("comment")
        // One of the comments must be available
        else if (startsMulti && multiLineComment(ctx)) ctx.pushAndContinue(())
        else if (startsMulti) ctx.fail("end of comment")
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

private [instructions] abstract class TokenLexi(name: String, illegalName: String)
                                               (start: TokenSet, letter: TokenSet, illegal: String => Boolean, _expected: UnsafeOption[String]) extends Instr {
    private val expected = if (_expected == null) name else _expected

    final override def apply(ctx: Context): Unit = {
        if (ctx.moreInput && start(ctx.nextChar)) {
            val name = new StringBuilder()
            name += ctx.nextChar
            ctx.offset += 1
            restOfToken(ctx, name)
        }
        else ctx.fail(expected)
    }

    private def ensureLegal(ctx: Context, tok: String) = {
        if (illegal(tok)) {
            ctx.offset -= tok.length
            ctx.unexpectedFail(expected = expected, unexpected = s"$illegalName $tok")
        }
        else {
            ctx.col += tok.length
            ctx.pushAndContinue(tok)
        }
    }

    @tailrec private final def restOfToken(ctx: Context, tok: StringBuilder): Unit = {
        if (ctx.moreInput && letter(ctx.nextChar)) {
            tok += ctx.nextChar
            ctx.offset += 1
            restOfToken(ctx, tok)
        }
        else ensureLegal(ctx, tok.toString)
    }

    // $COVERAGE-OFF$
    final override def toString: String = s"TokenLexi($name)"
    // $COVERAGE-ON$
}

private [internal] final class TokenIdentifier(start: TokenSet, letter: TokenSet, keywords: Set[String], _expected: UnsafeOption[String])
    extends TokenLexi("identifier", "keyword")(start, letter, keywords, _expected)

private [internal] final class TokenUserOperator(start: TokenSet, letter: TokenSet, reservedOps: Set[String], _expected: UnsafeOption[String])
    extends TokenLexi("operator", "reserved operator")(start, letter, reservedOps, _expected)

private [internal] final class TokenOperator(start: TokenSet, letter: TokenSet, reservedOps: Set[String], _expected: UnsafeOption[String])
    extends TokenLexi("operator", "non-reserved operator")(start, letter, reservedOps.andThen(!_), _expected)

private [instructions] abstract class TokenSpecific(_specific: String, caseSensitive: Boolean, _expected: UnsafeOption[String]) extends Instr {
    private final val expected = if (_expected == null) _specific else _expected
    protected final val expectedEnd = if (_expected == null) "end of " + _specific else _expected
    private final val specific = (if (caseSensitive) _specific else _specific.toLowerCase).toCharArray
    private final val strsz = specific.length
    protected def postprocess(ctx: Context, i: Int): Unit

    val readCharCaseHandled = {
        if (caseSensitive) (ctx: Context, i: Int) => ctx.input(i)
        else (ctx: Context, i: Int) => ctx.input(i).toLower
    }

    @tailrec final private def readSpecific(ctx: Context, i: Int, j: Int): Unit = {
        if (j < strsz && readCharCaseHandled(ctx, i) == specific(j)) readSpecific(ctx, i + 1, j + 1)
        else if (j < strsz) ctx.fail(expected)
        else {
            ctx.saveState()
            ctx.fastUncheckedConsumeChars(strsz)
            postprocess(ctx, i)
        }
    }

    final override def apply(ctx: Context): Unit = {
        if (ctx.inputsz >= ctx.offset + strsz) readSpecific(ctx, ctx.offset, 0)
        else ctx.fail(expected)
    }

    // $COVERAGE-OFF$
    override def toString: String = s"TokenSpecific(${_specific})"
    // $COVERAGE-ON$
}

private [internal] abstract class TokenSpecificNoTrailLetter(keyword: String, letter: TokenSet, caseSensitive: Boolean, expected: UnsafeOption[String])
    extends TokenSpecific(keyword, caseSensitive, expected) {
    final override def postprocess(ctx: Context, i: Int): Unit = {
        if (i < ctx.inputsz && letter(ctx.input(i))) {
            ctx.fail(expectedEnd)
            ctx.restoreState()
        }
        else {
            ctx.states = ctx.states.tail
            ctx.pushAndContinue(())
        }
    }
}

private [internal] final class TokenKeyword(keyword: String, letter: TokenSet, caseSensitive: Boolean, expected: UnsafeOption[String])
    extends TokenSpecificNoTrailLetter(keyword, letter, caseSensitive, expected)

private [internal] final class TokenOperator_(operator: String, letter: TokenSet, expected: UnsafeOption[String])
    extends TokenSpecificNoTrailLetter(operator, letter, true, expected)

private [internal] final class TokenMaxOp(operator: String, _ops: Set[String], expected: UnsafeOption[String])
    extends TokenSpecific(operator, true, expected) {
    private val ops = Radix(_ops.collect {
        case op if op.length > operator.length && op.startsWith(operator) => op.substring(operator.length)
    })

    @tailrec private def go(ctx: Context, i: Int, ops: Radix[Unit]): Unit = {
        lazy val ops_ = ops.suffixes(ctx.input(i))
        val possibleOpsRemain = i < ctx.inputsz && ops.nonEmpty
        if (possibleOpsRemain && ops_.contains("")) {
            ctx.fail(expectedEnd)
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
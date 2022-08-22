/* SPDX-FileCopyrightText: © 2018 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine

import scala.annotation.tailrec

//import parsley.{Failure, Result, Success} // not sure why this fails scalacheck, but I guess we'll leave it until I can submit a bug report
import parsley.Failure
import parsley.Result
import parsley.Success
import parsley.errors.ErrorBuilder

import parsley.internal.errors.{ExpectItem, Desc, LineBuilder}
import parsley.internal.machine.errors.{
    ClassicExpectedError, ClassicExpectedErrorWithReason, ClassicFancyError, ClassicUnexpectedError, DefuncError,
    DefuncHints, EmptyHints, ErrorItemBuilder, TokenError
}

import instructions.Instr
import stacks.{ArrayStack, CallStack, CheckStack, ErrorStack, HandlerStack, HintStack, Stack, StateStack}, Stack.StackExt

private [parsley] final class Context(private [machine] var instrs: Array[Instr],
                                      private [machine] val input: String,
                                      numRegs: Int,
                                      private val sourceFile: Option[String]) {
    /** This is the operand stack, where results go to live  */
    private [machine] val stack: ArrayStack[Any] = new ArrayStack()
    /** Current offset into the input */
    private [machine] var offset: Int = 0
    /** The length of the input, stored for whatever reason */
    private [machine] val inputsz: Int = input.length
    /** Call stack consisting of Frames that track the return position and the old instructions */
    private var calls: CallStack = Stack.empty
    /** State stack consisting of offsets and positions that can be rolled back */
    private [machine] var states: StateStack = Stack.empty
    /** Stack consisting of offsets at previous checkpoints, which may query to test for consumed input */
    private [machine] var checkStack: CheckStack = Stack.empty
    /** Current operational status of the machine */
    // TODO: turn into two bools? would make the runParser loop tighter
    private [machine] var status: Status = Good
    /** Stack of handlers, which track the call depth, program counter and stack size of error handlers */
    private [machine] var handlers: HandlerStack = Stack.empty
    /** Current size of the call stack */
    private var depth: Int = 0
    /** Current offset into program instruction buffer */
    private [machine] var pc: Int = 0
    /** Current line number */
    private [machine] var line: Int = 1
    /** Current column number */
    private [machine] var col: Int = 1
    /** State held by the registers, AnyRef to allow for `null` */
    private [machine] var regs: Array[AnyRef] = new Array[AnyRef](numRegs)
    /** Amount of indentation to apply to debug combinators output */
    private [machine] var debuglvl: Int = 0

    // NEW ERROR MECHANISMS
    private var hints: DefuncHints = EmptyHints
    private var hintsValidOffset = 0
    private var hintStack = Stack.empty[HintStack]
    private [machine] var errs: ErrorStack = Stack.empty

    private [machine] def saveHints(shadow: Boolean): Unit = {
        hintStack = new HintStack(hints, hintsValidOffset, hintStack)
        if (!shadow) hints = EmptyHints
    }
    private [machine] def restoreHints(): Unit = {
        val hintFrame = this.hintStack
        this.hintsValidOffset = hintFrame.validOffset
        this.hints = hintFrame.hints
        this.commitHints()
    }
    private [machine] def commitHints(): Unit = {
        this.hintStack = this.hintStack.tail
    }

    /* Error Debugging Info */
    private [machine] def inFlightHints: DefuncHints = hints
    private [machine] def inFlightError: DefuncError = errs.error
    private [machine] def currentHintsValidOffset: Int = hintsValidOffset

    /* ERROR RELABELLING BEGIN */
    private [machine] def mergeHints(): Unit = {
        val hintFrame = this.hintStack
        if (hintFrame.validOffset == offset) this.hints = hintFrame.hints.merge(this.hints)
        commitHints()
    }
    private [machine] def replaceHint(label: String): Unit = hints = hints.rename(label)
    private [machine] def popHints: Unit = hints = hints.pop
    /* ERROR RELABELLING END */

    private def addErrorToHints(): Unit = {
        val err = errs.error
        assume(!(!err.isExpectedEmpty) || err.isTrivialError, "not having an empty expected implies you are a trivial error")
        if (/*err.isTrivialError && */ !err.isExpectedEmpty && err.offset == offset) { // scalastyle:ignore disallow.space.after.token
            // If our new hints have taken place further in the input stream, then they must invalidate the old ones
            if (hintsValidOffset < offset) {
                hints = EmptyHints
                hintsValidOffset = offset
            }
            hints = hints.addError(err)
        }
    }
    private [machine] def addErrorToHintsAndPop(): Unit = {
        this.addErrorToHints()
        this.errs = this.errs.tail
    }

    private [machine] def updateCheckOffsetAndHints() = {
        this.checkStack.offset = this.offset
        this.hintsValidOffset = this.offset
    }

    // $COVERAGE-OFF$
    private [machine] def pretty: String = {
        s"""[
           |  stack     = [${stack.mkString(", ")}]
           |  instrs    = ${instrs.mkString("; ")}
           |  input     = ${input.drop(offset).mkString}
           |  pos       = ($line, $col)
           |  status    = $status
           |  pc        = $pc
           |  depth     = $depth
           |  rets      = ${calls.mkString(", ")}
           |  handlers  = ${handlers.mkString(", ")}
           |  recstates = ${states.mkString(", ")}
           |  checks    = ${checkStack.mkString(", ")}
           |  registers = ${regs.zipWithIndex.map{case (r, i) => s"r$i = $r"}.mkString("\n              ")}
           |  errors    = ${errs.mkString(", ")}
           |  hints     = ($hintsValidOffset, ${hints.toSet}):${hintStack.mkString(", ")}
           |]""".stripMargin
    }
    // $COVERAGE-ON$

    @tailrec private [parsley] def runParser[Err: ErrorBuilder, A](): Result[Err, A] = {
        //println(pretty)
        if (status eq Failed) {
            assert(!errs.isEmpty && errs.tail.isEmpty, "there should be exactly 1 parse error remaining at end of parse")
            assert(handlers.isEmpty, "there must be no more handlers on end of parse")
            assert(checkStack.isEmpty, "there must be no residual check remaining on end of parse")
            assert(states.isEmpty, "there must be no residual states left at end of parse")
            assert(hintStack.isEmpty, "there should be no hints remaining at end of parse")
            Failure(errs.error.asParseError().format(sourceFile))
        }
        else if (status ne Finished) {
            instrs(pc)(this)
            runParser[Err, A]()
        }
        else {
            assert(stack.size == 1, "stack must end a parse with exactly one item")
            assert(calls.isEmpty, "there must be no more calls to unwind on end of parser")
            assert(handlers.isEmpty, "there must be no more handlers on end of parse")
            assert(checkStack.isEmpty, "there must be no residual check remaining on end of parse")
            assert(states.isEmpty, "there must be no residual states left at end of parse")
            assert(errs.isEmpty, "there should be no parse errors remaining at end of parse")
            assert(hintStack.isEmpty, "there should be no hints remaining at end of parse")
            Success(stack.peek[A])
        }
    }

    private [machine] def call(newInstrs: Array[Instr]): Unit = {
        call(0)
        instrs = newInstrs
    }

    private [machine] def call(at: Int): Unit = {
        calls = new CallStack(pc + 1, instrs, at, calls)
        pc = at
        depth += 1
    }

    private [machine] def ret(): Unit = {
        assert(depth >= 1, "cannot return when no calls are made")
        instrs = calls.instrs
        pc = calls.ret
        calls = calls.tail
        depth -= 1
    }

    /** This method returns multiple times (in the case of a failure handler back many call-frames).
      *
      * @param n the number of frames to unwind.
      */
    private def multiRet(n: Int): Unit = if (n > 0) {
        if (n == 1) ret()
        else {
            var m = n - 1 // scalastyle:ignore var.local
            assert(depth >= m, "cannot return when no calls are made")
            depth -= m
            // the rollback can safely discard n-1 frames immediately, as stateful instructions are no longer a thing!
            while (m > 0) {
                calls = calls.tail
                m -= 1
            }
            // this does the final, not shortcutted return
            ret()
        }
    }

    private [machine] def catchNoConsumed(handler: =>Unit): Unit = {
        assert(status eq Recover, "catching can only be performed in a handler")
        if (offset != checkStack.offset) fail()
        else {
            status = Good
            handler
        }
        checkStack = checkStack.tail
    }

    private def pushError(err: DefuncError): Unit = this.errs = new ErrorStack(this.useHints(err), this.errs)
    private [machine] def useHints(err: DefuncError): DefuncError = {
        if (hintsValidOffset == err.offset) err.withHints(hints)
        else {
            hintsValidOffset = err.offset
            hints = EmptyHints
            err
        }
    }

    private [machine] def failWithMessage(msgs: String*): Unit = this.fail(new ClassicFancyError(offset, line, col, msgs: _*))
    private [machine] def unexpectedFail(expected: Option[ExpectItem], unexpected: Desc): Unit = {
        this.fail(new ClassicUnexpectedError(offset, line, col, expected, unexpected))
    }
    private [machine] def expectedFail(expected: Option[ExpectItem]): Unit = this.fail(new ClassicExpectedError(offset, line, col, expected))
    private [machine] def expectedFail(expected: Option[ExpectItem], reason: String): Unit = {
        this.fail(new ClassicExpectedErrorWithReason(offset, line, col, expected, reason))
    }
    private [machine] def expectedTokenFail(expected: Option[ExpectItem], size: Int): Unit = this.fail(new TokenError(offset, line, col, expected, size))

    private [machine] def fail(error: DefuncError): Unit = {
        this.pushError(error)
        this.fail()
    }
    private [machine] def fail(): Unit = {
        if (handlers.isEmpty) status = Failed
        else {
            status = Recover
            val handler = handlers
            handlers = handlers.tail
            multiRet(depth - handler.depth)
            pc = handler.pc
            val diffstack = stack.usize - handler.stacksz
            if (diffstack > 0) stack.drop(diffstack)
        }
    }

    private [machine] def pushAndContinue(x: Any) = {
        stack.push(x)
        inc()
    }
    private [machine] def unsafePushAndContinue(x: Any) = {
        stack.upush(x)
        inc()
    }
    private [machine] def exchangeAndContinue(x: Any) = {
        stack.exchange(x)
        inc()
    }
    private [machine] def inc(): Unit = pc += 1
    private [machine] def nextChar: Char = input.charAt(offset)
    private [machine] def moreInput: Boolean = offset < inputsz
    private [machine] def updatePos(c: Char) = c match {
        case '\n' => line += 1; col = 1
        case '\t' => col = ((col + 3) & -4) | 1//((col - 1) | 3) + 2
        case _    => col += 1
    }
    private [machine] def consumeChar(): Char = {
        val c = nextChar
        updatePos(c)
        offset += 1
        c
    }
    private [machine] def fastUncheckedConsumeChars(n: Int) = {
        offset += n
        col += n
    }
    private [machine] def pushHandler(label: Int): Unit = handlers = new HandlerStack(depth, label, stack.usize, handlers)
    private [machine] def pushCheck(): Unit = checkStack = new CheckStack(offset, checkStack)
    private [machine] def saveState(): Unit = states = new StateStack(offset, line, col, states)
    private [machine] def restoreState(): Unit = {
        val state = states
        states = states.tail
        offset = state.offset
        line = state.line
        col = state.col
    }
    private [machine] def writeReg(reg: Int, x: Any): Unit = {
        regs(reg) = x.asInstanceOf[AnyRef]
    }

    private implicit val lineBuilder: LineBuilder = new LineBuilder {
        def nearestNewlineBefore(off: Int): Option[Int] = {
            if (off < 0) None
            else Some {
                val idx = Context.this.input.lastIndexOf('\n', off-1)
                if (idx == -1) 0 else idx + 1
            }
        }
        def nearestNewlineAfter(off: Int): Option[Int] = {
            if (off > Context.this.inputsz) None
            else Some {
                val idx = Context.this.input.indexOf('\n', off)
                if (idx == -1) Context.this.inputsz else idx
            }
        }
        def segmentBetween(start: Int, end: Int): String = {
            Context.this.input.substring(start, end)
        }
    }

    private [machine] implicit val errorItemBuilder: ErrorItemBuilder = new ErrorItemBuilder {
        def inRange(offset: Int): Boolean = offset < Context.this.inputsz
        def charAt(offset: Int): Char = Context.this.input.charAt(offset)
        //def substring(offset: Int, size: Int): String = Context.this.input.substring(offset, Math.min(offset + size, Context.this.inputsz))
        def iterableFrom(offset: Int): Iterable[Char] = Context.this.input.substring(offset)
    }
}

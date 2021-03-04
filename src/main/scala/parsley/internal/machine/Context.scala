package parsley.internal.machine

import instructions.Instr
import stacks.{ArrayStack, Stack, CallStack, CheckStack, HandlerStack, StateStack, HintStack, ErrorStack}, Stack.StackExt
import parsley.{Failure, Result, Success}
import parsley.internal.errors.{ErrorItem, LineBuilder}
import parsley.internal.machine.errors.{
    ErrorItemBuilder,
    DefuncError, ClassicExpectedError, ClassicExpectedErrorWithReason, ClassicFancyError, ClassicUnexpectedError, WithHints,
    DefuncHints, EmptyHints, MergeHints, ReplaceHint, PopHints, AddError
}

import scala.annotation.tailrec
import scala.collection.mutable

private [parsley] object Context {
    private [Context] val NumRegs = 4
    private [parsley] def empty: Context = new Context(null, "")
}

private [parsley] final class Context(private [machine] var instrs: Array[Instr],
                                      private [machine] var input: String,
                                      private val sourceName: Option[String] = None) {
    /** This is the operand stack, where results go to live  */
    private [machine] val stack: ArrayStack[Any] = new ArrayStack()
    /** Current offset into the input */
    private [machine] var offset: Int = 0
    /** The length of the input, stored for whatever reason */
    private [machine] var inputsz: Int = input.length
    /** Call stack consisting of Frames that track the return position and the old instructions */
    //private var calls: FastStack[Frame] = FastStack.empty
    private var calls: CallStack = Stack.empty
    /** State stack consisting of offsets and positions that can be rolled back */
    private [machine] var states: StateStack = Stack.empty
    /** Stack consisting of offsets at previous checkpoints, which may query to test for consumed input */
    private [machine] var checkStack: CheckStack = Stack.empty
    /** Current operational status of the machine */
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
    private [machine] val regs: Array[AnyRef] = new Array[AnyRef](Context.NumRegs)
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

    /* ERROR RELABELLING BEGIN */
    private [machine] def mergeHints(): Unit = {
        val hintFrame = this.hintStack
        if (hintFrame.validOffset == offset) this.hints = MergeHints(hintFrame.hints, this.hints)
        commitHints()
    }
    private [machine] def replaceHint(label: String): Unit = hints = ReplaceHint(label, hints)
    private [machine] def popHints: Unit = hints = PopHints(hints)
    /* ERROR RELABELLING END */

    private def addErrorToHints(): Unit = {
        val err = errs.error
        if (err.isTrivialError && err.offset == offset && !err.isExpectedEmpty) {
            // If our new hints have taken place further in the input stream, then they must invalidate the old ones
            if (hintsValidOffset < offset) {
                hints = EmptyHints
                hintsValidOffset = offset
            }
            hints = new AddError(hints, err)
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

    @tailrec @inline private [parsley] def runParser[A](): Result[A] = {
        //println(pretty)
        if (status eq Failed) Failure(errs.error.asParseError.pretty(sourceName))
        else if (pc < instrs.length) {
            instrs(pc)(this)
            runParser[A]()
        }
        else if (calls.isEmpty) Success(stack.peek[A])
        else {
            ret()
            runParser[A]()
        }
    }

    private [machine] def call(newInstrs: Array[Instr], at: Int) = {
        calls = new CallStack(pc + 1, instrs, calls)
        instrs = newInstrs
        pc = at
        depth += 1
    }

    private [machine] def ret(): Unit = {
        instrs = calls.instrs
        pc = calls.ret
        calls = calls.tail
        depth -= 1
    }

    private [machine] def catchNoConsumed(handler: =>Unit): Unit = {
        if (offset != checkStack.offset) fail()
        else {
            status = Good
            handler
        }
        checkStack = checkStack.tail
    }

    private [machine] def pushError(err: DefuncError): Unit = this.errs = new ErrorStack(this.useHints(err), this.errs)
    private [machine] def useHints(err: DefuncError): DefuncError = {
        if (hintsValidOffset == offset) WithHints(err, hints)
        else {
            hintsValidOffset = offset
            hints = EmptyHints
            err
        }
    }

    private [machine] def failWithMessage(msg: String): Unit = {
        this.fail(new ClassicFancyError(offset, line, col, msg))
    }
    private [machine] def unexpectedFail(expected: Option[ErrorItem], unexpected: ErrorItem): Unit = {
        this.fail(new ClassicUnexpectedError(offset, line, col, expected, unexpected))
    }
    private [machine] def expectedFail(expected: Option[ErrorItem]): Unit = {
        this.fail(new ClassicExpectedError(offset, line, col, expected))
    }
    private [machine] def expectedFail(expected: Option[ErrorItem], reason: String): Unit = {
        this.fail(new ClassicExpectedErrorWithReason(offset, line, col, expected, reason))
    }
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
            pc = handler.pc
            val diffstack = stack.usize - handler.stacksz
            val diffdepth = depth - handler.depth - 1
            depth = handler.depth
            if (diffdepth >= 0) {
                val calls_ = CallStack.drop(calls, diffdepth)
                instrs = calls_.instrs
                calls = calls_.tail
            }
            if (diffstack > 0) stack.drop(diffstack)
        }
    }

    private [machine] def pushAndContinue(x: Any) = {
        stack.push(x)
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
        case '\t' => col += 4 - ((col - 1) & 3)
        case _ => col += 1
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

    // Allows us to reuse a context, helpful for benchmarking and potentially user applications
    private [parsley] def apply(_instrs: Array[Instr], _input: String): Context = {
        instrs = _instrs
        input = _input
        stack.clear()
        offset = 0
        inputsz = input.length
        calls = Stack.empty
        states = Stack.empty
        checkStack = Stack.empty
        status = Good
        handlers = Stack.empty
        depth = 0
        pc = 0
        line = 1
        col = 1
        debuglvl = 0
        hintsValidOffset = 0
        hints = EmptyHints
        hintStack = Stack.empty
        this
    }

    private implicit val lineBuilder: LineBuilder = new LineBuilder {
        def nearestNewlineBefore(off: Int): Int = {
            val idx = Context.this.input.lastIndexOf('\n', off-1)
            if (idx == -1) 0 else idx + 1
        }
        def nearestNewlineAfter(off: Int): Int = {
            val idx = Context.this.input.indexOf('\n', off)
            if (idx == -1) Context.this.inputsz else idx
        }
        def segmentBetween(start: Int, end: Int): String = {
            Context.this.input.substring(start, end)
        }
    }

    private implicit val errorItemBuilder: ErrorItemBuilder = new ErrorItemBuilder {
        def inRange(offset: Int): Boolean = offset < Context.this.inputsz
        def charAt(offset: Int): Char = Context.this.input.charAt(offset)
        def substring(offset: Int, size: Int): String = Context.this.input.substring(offset, Math.min(offset + size, Context.this.inputsz))
    }
}
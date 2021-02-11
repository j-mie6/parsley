package parsley.internal.instructions

import Stack.{drop, isEmpty, mkString, map, push}
import parsley.{Failure, Result, Success}
import parsley.internal.UnsafeOption

import scala.annotation.tailrec
import scala.collection.mutable

// Private internals
private [instructions] final class Frame(val ret: Int, val instrs: Array[Instr]) {
    override def toString: String = s"[$instrs@$ret]"
}
private [instructions] final class Handler(val depth: Int, val pc: Int, var stacksz: Int) {
    override def toString: String = s"Handler@$depth:$pc(-${stacksz + 1})"
}
private [instructions] final class State(val offset: Int, val line: Int, val col: Int) {
    override def toString: String = s"$offset ($line, $col)"
}

private [parsley] final class Context(private [instructions] var instrs: Array[Instr],
                                      private [instructions] var input: Array[Char],
                                      private [instructions] val sourceName: Option[String] = None) {
    /** This is the operand stack, where results go to live  */
    private [instructions] val stack: ArrayStack[Any] = new ArrayStack()
    /** Current offset into the input */
    private [instructions] var offset: Int = 0
    /** The length of the input, stored for whatever reason */
    private [instructions] var inputsz: Int = input.length
    /** Call stack consisting of Frames that track the return position and the old instructions */
    private [instructions] var calls: Stack[Frame] = Stack.empty
    /** State stack consisting of offsets and positions that can be rolled back */
    private [instructions] var states: Stack[State] = Stack.empty
    /** Stack consisting of offsets at previous checkpoints, which may query to test for consumed input */
    private [instructions] var checkStack: Stack[Int] = Stack.empty
    /** Current operational status of the machine */
    private [instructions] var status: Status = Good
    /** Stack of handlers, which track the call depth, program counter and stack size of error handlers */
    private [instructions] var handlers: Stack[Handler] = Stack.empty
    /** Current size of the call stack */
    private [instructions] var depth: Int = 0
    /** Current offset into program instruction buffer */
    private [instructions] var pc: Int = 0
    /** Current line number */
    private [instructions] var line: Int = 1
    /** Current column number */
    private [instructions] var col: Int = 1
    /** State held by the registers, AnyRef to allow for `null` */
    private [instructions] var regs: Array[AnyRef] = new Array[AnyRef](Context.NumRegs)
    /** Amount of indentation to apply to debug combinators output */
    private [instructions] var debuglvl: Int = 0
    /** Name which describes the type of input in error messages */
    private val inputDescriptor = sourceName.fold("input")(_ => "file")

    // NEW ERROR MECHANISMS
    private var hints = mutable.ListBuffer.empty[Hint]
    private var hintsValidOffset = 0
    private var hintStack = Stack.empty[(Int, mutable.ListBuffer[Hint])]
    private [instructions] var errs = Stack.empty[ParseError]

    private [instructions] def saveHints(shadow: Boolean): Unit = {
        hintStack = push(hintStack, (hintsValidOffset, hints))
        hints = if (shadow) hints.clone else mutable.ListBuffer.empty
    }
    private [instructions] def restoreHints(): Unit = {
        val (hintsValidOffset, hints) = hintStack.head
        this.hintsValidOffset = hintsValidOffset
        this.hints = hints
        this.commitHints()
    }
    private [instructions] def commitHints(): Unit = {
        this.hintStack = this.hintStack.tail
    }

    /* ERROR RELABELLING BEGIN */
    private [instructions] def mergeHints(): Unit = {
        val newHints = this.hints
        val (hintsValidOffset, oldHints) = this.hintStack.head
        if (hintsValidOffset == offset) {
            this.hints = oldHints
            this.hints ++= newHints
        }
        commitHints()
    }
    private [instructions] def replaceHint(label: String): Unit = {
        if (hints.nonEmpty) hints(0) = new Hint(Set(Desc(label)))
    }
    private [instructions] def popHints: Unit = if (hints.nonEmpty) hints.remove(0)
    /* ERROR RELABELLING END */

    private [instructions] def addErrorToHints(): Unit = errs.head match {
        case TrivialError(errOffset, _, _, _, es, _) if errOffset == offset && es.nonEmpty =>
            // If our new hints have taken place further in the input stream, then they must invalidate the old ones
            if (hintsValidOffset < offset) hints.clear()
            hintsValidOffset = offset
            hints += new Hint(es)
        case _ =>
    }
    private [instructions] def addErrorToHintsAndPop(): Unit = {
        this.addErrorToHints()
        this.errs = this.errs.tail
    }

    private [instructions] def updateCheckOffsetAndHints() = {
        this.checkStack.head = this.offset
        this.hintsValidOffset = this.offset
    }

    // $COVERAGE-OFF$
    private [instructions] def pretty: String = {
        s"""[
           |  stack     = [${stack.mkString(", ")}]
           |  instrs    = ${instrs.mkString("; ")}
           |  input     = ${input.drop(offset).mkString}
           |  pos       = ($line, $col)
           |  status    = $status
           |  pc        = $pc
           |  depth     = $depth
           |  rets      = ${mkString(map[Frame, Int](calls, _.ret), ", ")}
           |  handlers  = ${mkString(handlers, ":")}[]
           |  recstates = ${mkString(states, ":")}[]
           |  checks    = ${mkString(checkStack, ":")}[]
           |  registers = ${regs.zipWithIndex.map{case (r, i) => s"r$i = $r"}.mkString("\n              ")}
           |  errors    = ${mkString(errs, ":")}[]
           |  hints     = ($hintsValidOffset, ${hints}):${mkString(hintStack, ":")}[]
           |]""".stripMargin
    }
    // $COVERAGE-ON$

    @tailrec @inline private [parsley] def runParser[A](): Result[A] = {
        //println(pretty)
        if (status eq Failed) {
            assert(!isEmpty(errs) && isEmpty(errs.tail), "there should be only one error on failure")
            assert(isEmpty(handlers), "there should be no handlers left on failure")
            assert(isEmpty(hintStack), "there should be at most one set of hints left at the end")
            Failure(errs.head.pretty(sourceName, new InputHelper))
        }
        else if (pc < instrs.length) {
            instrs(pc)(this)
            runParser[A]()
        }
        else if (isEmpty(calls)) {
            assert(isEmpty(errs), "there should be no errors on success")
            assert(isEmpty(handlers), "there should be no handlers on success")
            assert(isEmpty(hintStack), "there should be at most one set of hints left at the end")
            Success(stack.peek[A])
        }
        else {
            ret()
            runParser[A]()
        }
    }

    private [instructions] def call(newInstrs: Array[Instr], at: Int) = {
        calls = push(calls, new Frame(pc + 1, instrs))
        instrs = newInstrs
        pc = at
        depth += 1
    }

    private [instructions] def ret(): Unit = {
        val frame = calls.head
        instrs = frame.instrs
        calls = calls.tail
        pc = frame.ret
        depth -= 1
    }

    private [instructions] def catchNoConsumed(handler: =>Unit): Unit = {
        if (offset != checkStack.head) fail()
        else {
            status = Good
            handler
        }
        checkStack = checkStack.tail
    }

    private [instructions] def pushError(err: ParseError): Unit = {
        this.errs = push(this.errs, err)
        this.useHints()
    }

    private [instructions] def useHints(): Unit = {
        if (hintsValidOffset == offset) {
            errs.head = errs.head.withHints(hints)
        }
        else {
            hintsValidOffset = offset
            hints.clear()
        }
    }

    private [instructions] def failWithMessage(msg: String): Unit = {
        this.fail(ParseError.fail(msg, offset, line, col))
    }
    private [instructions] def unexpectedFail(expected: UnsafeOption[String], unexpected: String): Unit = {
        this.fail(TrivialError(offset, line, col, Some(Desc(unexpected)), if (expected == null) Set.empty else Set(Desc(expected)), Set.empty))
    }
    private [instructions] def expectedFail(expected: Set[ErrorItem], msg: Option[String]): Unit = {
        val unexpected = if (offset < inputsz) Raw(s"$nextChar") else EndOfInput
        this.fail(TrivialError(offset, line, col, Some(unexpected), expected, msg.fold(Set.empty[String])(Set(_))))
    }
    private [instructions] def expectedFail(expected: UnsafeOption[String]): Unit = {
        expectedFail(if (expected == null) Set.empty[ErrorItem] else Set[ErrorItem](Desc(expected)), None)
    }
    private [instructions] def expectedFailWithExplanation(expected: UnsafeOption[String], msg: String): Unit = {
        expectedFail(if (expected == null) Set.empty[ErrorItem] else Set[ErrorItem](Desc(expected)), Some(msg))
    }
    private [instructions] def fail(error: ParseError): Unit = {
        this.pushError(error)
        this.fail()
    }
    private [instructions] def fail(): Unit = {
        if (isEmpty(handlers)) status = Failed
        else {
            status = Recover
            val handler = handlers.head
            handlers = handlers.tail
            val diffdepth = depth - handler.depth - 1
            if (diffdepth >= 0) {
                val calls_ = drop(calls, diffdepth)
                instrs = calls_.head.instrs
                calls = calls_.tail
            }
            pc = handler.pc
            val diffstack = stack.usize - handler.stacksz
            if (diffstack > 0) stack.drop(diffstack)
            depth = handler.depth
        }
    }

    private [instructions] def pushAndContinue(x: Any) = {
        stack.push(x)
        inc()
    }
    private [instructions] def exchangeAndContinue(x: Any) = {
        stack.exchange(x)
        inc()
    }
    private [instructions] def inc(): Unit = pc += 1
    private [instructions] def nextChar: Char = input(offset)
    private [instructions] def moreInput: Boolean = offset < inputsz
    private [instructions] def updatePos(c: Char) = c match {
        case '\n' => line += 1; col = 1
        case '\t' => col += 4 - ((col - 1) & 3)
        case _ => col += 1
    }
    private [instructions] def consumeChar(): Char = {
        val c = nextChar
        updatePos(c)
        offset += 1
        c
    }
    private [instructions] def fastUncheckedConsumeChars(n: Int) = {
        offset += n
        col += n
    }
    private [instructions] def pushHandler(label: Int): Unit = {
        handlers = push(handlers, new Handler(depth, label, stack.usize))
        //TODO: This may change
        //this.saveHints()
    }
    private [instructions] def pushCheck(): Unit = checkStack = push(checkStack, offset)
    private [instructions] def saveState(): Unit = states = push(states, new State(offset, line, col))
    private [instructions] def restoreState(): Unit = {
        val state = states.head
        states = states.tail
        offset = state.offset
        line = state.line
        col = state.col
    }
    private [instructions] def writeReg(reg: Int, x: Any): Unit = {
        regs(reg) = x.asInstanceOf[AnyRef]
    }

    // Allows us to reuse a context, helpful for benchmarking and potentially user applications
    private [parsley] def apply(_instrs: Array[Instr], _input: Array[Char]): Context = {
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
        this
    }

    private [instructions] class InputHelper {
        def nearestNewlineBefore(off: Int): Int = {
            val idx = Context.this.input.lastIndexOf('\n', off-1)
            if (idx == -1) 0 else idx + 1
        }
        def nearestNewlineAfter(off: Int): Int = {
            val idx = Context.this.input.indexOf('\n', off)
            if (idx == -1) Context.this.inputsz else idx
        }
        def segmentBetween(start: Int, end: Int): String = {
            Context.this.input.slice(start, end).mkString
        }
    }
}

private [parsley] object Context {
    private [Context] val NumRegs = 4
    def empty: Context = new Context(null, Array.emptyCharArray)
}
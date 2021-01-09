package parsley.internal.instructions

import Stack.{drop, isEmpty, mkString, map, push}
import parsley.{Failure, Result, Success}
import parsley.internal.UnsafeOption

import scala.annotation.tailrec

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
                                      private [instructions] var input: Array[Char]) {
    private [instructions] val stack: ArrayStack[Any] = new ArrayStack()
    private [instructions] var offset: Int = 0
    private [instructions] var inputsz: Int = input.length
    private [instructions] var calls: Stack[Frame] = Stack.empty
    private [instructions] var states: Stack[State] = Stack.empty
    private [instructions] var checkStack: Stack[Int] = Stack.empty
    private [instructions] var status: Status = Good
    private [instructions] var handlers: Stack[Handler] = Stack.empty
    private [instructions] var depth: Int = 0
    private [instructions] var pc: Int = 0
    private [instructions] var line: Int = 1
    private [instructions] var col: Int = 1
    private [instructions] var erroffset: Int = -1
    private [instructions] var errcol: Int = -1
    private [instructions] var errline: Int = -1
    private [instructions] var raw: List[String] = Nil
    private [instructions] var unexpected: UnsafeOption[String] = _
    private [instructions] var expected: List[UnsafeOption[String]] = Nil
    private [instructions] var unexpectAnyway: Boolean = false
    private [instructions] var errorOverride: UnsafeOption[String] = _
    private [instructions] var overrideDepth: Int = 0
    private [instructions] var regs: Array[AnyRef] = new Array[AnyRef](Context.NumRegs)
    private [instructions] var debuglvl: Int = 0
    private [instructions] var startline: Int = 1
    private [instructions] var startcol: Int = 1
    private [parsley] var sourceName: String = "input"

    // $COVERAGE-OFF$
    //override def toString: String = pretty
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
           |]""".stripMargin
    }
    // $COVERAGE-ON$

    /*private [parsley] def pos: (Int, Int) = (startline, startcol)
    private [parsley] def pos_=(pos: (Int, Int)): Unit = {
        val (line, col) = pos
        startline = line
        startcol = col
    }*/

    @tailrec @inline private [parsley] def runParser[A](): Result[A] = {
        //println(this)
        if (status eq Failed) Failure(errorMessage)
        else if (pc < instrs.length) {
            instrs(pc)(this)
            runParser[A]()
        }
        else if (isEmpty(calls)) Success(stack.peek[A])
        else {
            ret()
            runParser[A]()
        }
    }

    private def adjustErrorOverride(): Unit = {
        if (depth < overrideDepth) {
            overrideDepth = 0
            errorOverride = null
        }
    }

    private [instructions] def call(newInstrs: Array[Instr], at: Int, expected: UnsafeOption[String]) = {
        calls = push(calls, new Frame(pc + 1, instrs))
        instrs = newInstrs
        pc = at
        depth += 1
        if (expected != null && errorOverride == null) {
            overrideDepth = depth
            errorOverride = expected
        }
    }

    private [instructions] def ret(): Unit = {
        val frame = calls.head
        instrs = frame.instrs
        calls = calls.tail
        pc = frame.ret
        depth -= 1
        adjustErrorOverride()
    }

    private [instructions] def catchNoConsumed(handler: =>Unit): Unit = {
        if (offset != checkStack.head) fail()
        else {
            status = Good
            handler
        }
        checkStack = checkStack.tail
    }

    private def adjustErrors(e: UnsafeOption[String]): Unit = {
        if (offset > erroffset) {
            erroffset = offset
            errcol = col
            errline = line
            unexpected = if (offset < inputsz) "\"" + nextChar + "\"" else "end of " + sourceName
            expected = (if (errorOverride == null) e else errorOverride)::Nil
            raw = Nil
            unexpectAnyway = false
        }
        else if (offset == erroffset) expected ::= (if (errorOverride == null) e else errorOverride)
        adjustErrorOverride()
    }

    private [instructions] def failWithMessage(expected: UnsafeOption[String], msg: String): Unit = {
        this.fail(expected)
        this.raw ::= msg
    }
    private [instructions] def unexpectedFail(expected: UnsafeOption[String], unexpected: String): Unit = {
        this.fail(expected)
        this.unexpected = unexpected
        this.unexpectAnyway = true
    }
    private [instructions] def fail(expected: UnsafeOption[String] = null): Unit = {
        if (isEmpty(handlers)) {
            status = Failed
            if (erroffset == -1) {
                errcol = col
                errline = line
            }
        }
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
        adjustErrors(expected)
    }

    private def errorMessage: String = {
        val posStr = s"(line $errline, column $errcol):"
        val unexpectedStr = Option(unexpected).map(s => s"unexpected $s")
        val expectedFlat = expected.flatMap(Option(_))
        val expectedFiltered = expectedFlat.filterNot(_.isEmpty)
        val rawFiltered = raw.filterNot(_.isEmpty)
        val expectedStr = if (expectedFiltered.isEmpty) None else Some(s"expected ${expectedFiltered.distinct.reverse.mkString(" or ")}")
        val rawStr = if (rawFiltered.isEmpty) None else Some(rawFiltered.distinct.reverse.mkString(" or "))
        unexpectAnyway ||= expectedFlat.nonEmpty || raw.nonEmpty
        if      (expectedStr.nonEmpty) s"$posStr${unexpectedStr.fold("")("\n  " + _)}\n  ${expectedStr.get}${rawStr.fold("")("\n  " + _)}"
        else if (rawStr.nonEmpty)      s"$posStr\n  ${rawStr.get}"
        else if (unexpectAnyway)       s"$posStr\n  ${unexpectedStr.getOrElse("unknown parse error")}"
        else                           s"$posStr\n  unknown parse error"
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
    private [instructions] def pushHandler(label: Int): Unit = handlers = push(handlers, new Handler(depth, label, stack.usize))
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
        line = startline
        col = startcol
        erroffset = -1
        errcol = -1
        errline = -1
        raw = Nil
        unexpected = null
        expected = Nil
        unexpectAnyway = false
        errorOverride = null
        overrideDepth = 0
        debuglvl = 0
        this
    }
}

private [parsley] object Context {
    private [Context] val NumRegs = 4
    def empty: Context = new Context(null, Array.emptyCharArray)
}
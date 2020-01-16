package parsley.instructions

import Stack._
import parsley.{Failure, Result, Success, UnsafeOption}

import scala.annotation.tailrec

// Private internals
private [instructions] final class Frame(val ret: Int, val instrs: Array[Instr])
{
    override def toString: String = s"[$instrs@$ret]"
}
private [instructions] final class Handler(val depth: Int, val pc: Int, val stacksz: Int)
{
    override def toString: String = s"Handler@$depth:$pc(-${stacksz+1})"
}
private [instructions] final class State(val offset: Int, val line: Int, val col: Int, val regs: Array[Any])
{
    override def toString: String = s"$offset ($line, $col)"
}

final class Context private [parsley] (private [instructions] var instrs: Array[Instr],
                                       private [instructions] var input: Array[Char])
{
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
    private [instructions] var regs: Array[Any] = new Array[Any](4)
    private [instructions] var debuglvl: Int = 0
    private [instructions] var startline: Int = 1
    private [instructions] var startcol: Int = 1
    var sourceName: String = "input"

    private [instructions] def pretty: String =
    {
        s"""[
           |  stack     = [${stack.mkString(", ")}]
           |  instrs    = ${instrs.mkString("; ")}
           |  input     = ${input.drop(offset).mkString}
           |  pos       = ($line, $col)
           |  status    = $status
           |  pc        = $pc
           |  depth     = $depth
           |  rets      = ${mkString(map[Frame, Int](calls, _.ret), ", ")}
           |  handlers  = ${mkString(handlers, ":") + "[]"}
           |  recstates = ${mkString(states, ":") + "[]"}
           |  checks    = ${mkString(checkStack, ":") + "[]"}
           |  registers = ${regs.zipWithIndex.map{case (r, i) => s"r$i = $r"}.mkString("\n              ")}
           |]""".stripMargin
    }

    def pos: (Int, Int) = (startline, startcol)
    def pos_=(pos: (Int, Int)): Unit =
    {
        val (line, col) = pos
        startline = line
        startcol = col
    }

    @tailrec @inline private [parsley] def runParser[A](): Result[A] =
    {
        //println(this)
        if (status eq Failed) return Failure(errorMessage)
        if (pc < instrs.length)
        {
            instrs(pc)(this)
            runParser[A]()
        }
        else if (isEmpty(calls)) Success(stack.peek[A])
        else
        {
            ret()
            runParser[A]()
        }
    }

    private [instructions] def ret(): Unit =
    {
        val frame = calls.head
        instrs = frame.instrs
        calls = calls.tail
        pc = frame.ret
        depth -= 1
        if (depth < overrideDepth)
        {
            overrideDepth = 0
            errorOverride = null
        }
    }

    private [instructions] def fail(e: UnsafeOption[String] = null): Unit =
    {
        if (isEmpty(handlers))
        {
            status = Failed
            if (erroffset == -1)
            {
                errcol = col
                errline = line
            }
        }
        else
        {
            status = Recover
            val handler = handlers.head
            handlers = handlers.tail
            val diffdepth = depth - handler.depth - 1
            if (diffdepth >= 0)
            {
                val calls_ = if (diffdepth != 0) drop(calls, diffdepth) else calls
                instrs = calls_.head.instrs
                calls = calls_.tail
            }
            pc = handler.pc
            val diffstack = stack.usize - handler.stacksz
            if (diffstack > 0) stack.drop(diffstack)
            depth = handler.depth
        }
        if (offset > erroffset)
        {
            erroffset = offset
            errcol = col
            errline = line
            unexpected = if (offset < inputsz) "\"" + nextChar + "\"" else "end of " + sourceName
            expected = (if (errorOverride == null) e else errorOverride)::Nil
            raw = Nil
            unexpectAnyway = false
        }
        else if (offset == erroffset) expected ::= (if (errorOverride == null) e else errorOverride)
        if (depth < overrideDepth)
        {
            overrideDepth = 0
            errorOverride = null
        }
    }

    private def errorMessage: String =
    {
        val posStr = s"(line $errline, column $errcol):"
        val unexpectedStr = Option(unexpected).map(s => s"unexpected $s")
        val expectedFlat = expected.flatMap(Option(_))
        val expectedStr = if (expectedFlat.isEmpty) None else Some(s"expected ${expectedFlat.distinct.reverse.mkString(" or ")}")
        val rawStr = if (raw.isEmpty) None else Some(raw.distinct.reverse.mkString(" or "))
        if (rawStr.isEmpty && expectedStr.isEmpty && unexpectAnyway) 
        {
            s"$posStr\n  ${unexpectedStr.getOrElse("unknown parse error")}"
        }
        else if (rawStr.isEmpty && expectedStr.isEmpty) 
        {
            s"$posStr\n  unknown parse error"
        }
        else if (expectedStr.isEmpty)
        {
            s"$posStr\n  ${rawStr.get}"
        }
        else
        {
            s"$posStr${unexpectedStr.fold("")("\n  " + _)}\n  ${expectedStr.get}${rawStr.fold("")("\n  " + _)}"
        }
    }

    private [instructions] def inc(): Unit = pc += 1
    private [instructions] def nextChar: Char = input(offset)
    private [instructions] def moreInput: Boolean = offset < inputsz
    
    // Allows us to reuse a context, helpful for benchmarking and potentially user applications
    private [parsley] def apply(_instrs: Array[Instr], _input: Array[Char]): Context = 
    {
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

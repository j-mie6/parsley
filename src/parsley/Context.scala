package parsley

import parsley.Stack._

// Private internals
private [parsley] final class Frame(val ret: Int, val instrs: Array[Instr])
{
    override def toString: String = s"[$instrs@$ret]"
}
private [parsley] final class Handler(val depth: Int, val pc: Int, val stacksz: Int)
{
    override def toString: String = s"Handler@$depth:$pc(-$stacksz)"
}
private [parsley] final class State(val offset: Int, val line: Int, val col: Int)
{
    override def toString: String = s"$offset ($line, $col)"
}

private [parsley] final class Context(private [parsley] var instrs: Array[Instr],
                                      private [parsley] var input: Array[Char])
{
    private [parsley] val stack: ArrayStack[Any] = new ArrayStack()
    private [parsley] var offset: Int = 0
    private [parsley] var inputsz: Int = input.length
    private [parsley] var calls: Stack[Frame] = Stack.empty
    private [parsley] var states: Stack[State] = Stack.empty
    private [parsley] var checkStack: Stack[Int] = Stack.empty
    private [parsley] var status: Status = Good
    private [parsley] var handlers: Stack[Handler] = Stack.empty
    private [parsley] var depth: Int = 0
    private [parsley] var pc: Int = 0
    private [parsley] var line: Int = 1
    private [parsley] var col: Int = 1
    private [parsley] var erroffset: Int = -1
    private [parsley] var errcol: Int = -1
    private [parsley] var errline: Int = -1
    private [parsley] var raw: List[String] = Nil
    private [parsley] var unexpected: UnsafeOption[String] = _
    private [parsley] var expected: List[UnsafeOption[String]] = Nil
    private [parsley] var unexpectAnyway: Boolean = false
    private [parsley] var errorOverride: UnsafeOption[String] = _
    private [parsley] var overrideDepth: Int = 0

    override def toString: String =
    {
        s"""|[
            |  stack=[${stack.mkString(", ")}]
            |  instrs=${instrs.mkString("; ")}
            |  input=${input.drop(offset).mkString}
            |  status=$status
            |  pc=$pc
            |  depth=$depth
            |  rets=${mkString(map[Frame, Int](calls, _.ret), ", ")}
            |  handlers=${mkString(handlers, ":") + "[]"}
            |  recstates=${mkString(handlers, ":") + "[]"}
            |]""".stripMargin
    }

    private [parsley] def fail(e: UnsafeOption[String] = null): Unit =
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
            val diffstack = stack.size - handler.stacksz
            if (diffstack > 0) stack.drop(diffstack)
            depth = handler.depth
        }
        if (offset > erroffset)
        {
            erroffset = offset
            errcol = col
            errline = line
            unexpected = if (offset < inputsz) "\"" + input(offset).toString + "\"" else "end of input"
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

    private [parsley] def errorMessage: String =
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

    private [parsley] def inc(): Unit = pc += 1
    
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
        line = 1
        col = 1
        erroffset = -1
        errcol = -1
        errline = -1
        raw = Nil
        unexpected = null
        expected = Nil
        unexpectAnyway = false
        errorOverride = null
        overrideDepth = 0
        this
    }
}

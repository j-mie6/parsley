package parsley

import parsley.Stack._

private [parsley] final class Context(var instrs: Array[Instr],
                                      var input: Input,
                                      var inputsz: Int,
                                      val subs: Map[String, Array[Instr]])
{
    var stack: Stack[Any] = Stack.empty
    var calls: CallStack = Stack.empty
    var states: StateStack = Stack.empty
    var stacksz: Int = 0
    var checkStack: Stack[Int] = Stack.empty
    var status: Status = Good
    var handlers: HandlerStack = Stack.empty // Doesn't favour indirect operations
    var depth: Int = 0
    var pc: ProgramCounter = 0

    override def toString: String =
    {
        s"""|[
            |  stack=[${mkString(stack, ", ")}]
            |  instrs=${instrs.mkString("; ")}
            |  input=${input.mkString(", ")}
            |  status=$status
            |  pc=$pc
            |  depth=$depth
            |  rets=${mkString(map[Frame, Int](calls, _.ret), ", ")}
            |  handlers=$handlers
            |  recstates=$states
            |]""".stripMargin
    }

    def fail()
    {
        if (isEmpty(handlers)) status = Failed
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
            val diffstack = stacksz - handler.stacksz
            if (diffstack > 0) stack = drop(stack, diffstack)
            stacksz = handler.stacksz
            depth = handler.depth
        }
    }

    def inc() { pc += 1 }

    // Stack Manipulation Methods
    def pushStack(x: Any) { stack = new Stack(x, stack); stacksz += 1 }
    def popStack(): Any =
    {
        val ret = stack.head
        stack = stack.tail
        stacksz -= 1
        ret
    }
    def exchangeStack(x: Any) { stack.head = x }
}

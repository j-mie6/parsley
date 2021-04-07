package parsley.internal.machine.stacks

import parsley.internal.machine.instructions.Instr
import scala.annotation.tailrec

private [machine] final class CallStack(val ret: Int, val instrs: Array[Instr], val exchange: Array[(Int, Instr)], val callId: Int, val tail: CallStack)
private [machine] object CallStack extends Stack[CallStack] {
    implicit val inst: Stack[CallStack] = this
    type ElemTy = (Int, Array[Instr])
    // $COVERAGE-OFF$
    override protected def show(x: ElemTy): String = x._1.toString
    override protected def head(xs: CallStack): ElemTy = (xs.ret, xs.instrs)
    override protected def tail(xs: CallStack): CallStack = xs.tail
    // $COVERAGE-ON$
    @tailrec final def drop(xs: CallStack, n: Int): CallStack = {
        if (n > 0 && !isEmpty(xs)) drop(xs.tail, n - 1) else xs
    }
}
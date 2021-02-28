package parsley.internal.machine.stacks

private [machine] final class HandlerStack(val depth: Int, val pc: Int, var stacksz: Int, val tail: HandlerStack)
object HandlerStack extends Stack[HandlerStack] {
    implicit val inst = this
    type ElemTy = (Int, Int, Int)
    override protected def show(x: ElemTy): String = {
        val (depth, pc, stacksz) = x
        s"Handler@$depth:$pc(-${stacksz + 1})"
    }
    override protected def head(xs: HandlerStack): ElemTy = (xs.depth, xs.pc, xs.stacksz)
    override protected def tail(xs: HandlerStack): HandlerStack = xs.tail
}
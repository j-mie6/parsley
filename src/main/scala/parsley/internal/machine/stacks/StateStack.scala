package parsley.internal.machine.stacks

private [machine] final class StateStack(val offset: Int, val line: Int, val col: Int, val tail: StateStack)
private [machine] object StateStack extends Stack[StateStack] {
    implicit val inst: Stack[StateStack] = this
    type ElemTy = (Int, Int, Int)
    // $COVERAGE-OFF$
    override protected def show(x: ElemTy): String = {
        val (offset, line, col) = x
        s"$offset ($line, $col)"
    }
    override protected def head(xs: StateStack): ElemTy = (xs.offset, xs.line, xs.col)
    override protected def tail(xs: StateStack): StateStack = xs.tail
    // $COVERAGE-ON$
}
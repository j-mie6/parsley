package parsley.internal.machine.stacks

import parsley.internal.errors.DefuncHints

private [machine] final class HintStack(val hints: DefuncHints, val validOffset: Int, val tail: HintStack)
object HintStack extends Stack[HintStack] {
    implicit val inst: Stack[HintStack] = this
    type ElemTy = (DefuncHints, Int)
    override protected def show(x: ElemTy): String = {
        val (hints, validOffset) = x
        s"($validOffset, $hints)"
    }
    override protected def head(xs: HintStack): ElemTy = (xs.hints, xs.validOffset)
    override protected def tail(xs: HintStack): HintStack = xs.tail
}

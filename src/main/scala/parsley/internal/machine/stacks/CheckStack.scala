package parsley.internal.machine.stacks

private [machine] final class CheckStack(var offset: Int, val tail: CheckStack)
object CheckStack extends Stack[CheckStack] {
    implicit val inst: Stack[CheckStack] = this
    type ElemTy = Int
    override protected def show(x: ElemTy): String = x.toString
    override protected def head(xs: CheckStack): ElemTy = xs.offset
    override protected def tail(xs: CheckStack): CheckStack = xs.tail
}
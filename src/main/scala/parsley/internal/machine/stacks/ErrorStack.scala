package parsley.internal.machine.stacks

import parsley.internal.errors.DefuncError

private [machine] final class ErrorStack(var error: DefuncError, val tail: ErrorStack)
private [machine] object ErrorStack extends Stack[ErrorStack] {
    implicit val inst: Stack[ErrorStack] = this
    type ElemTy = DefuncError
    // $COVERAGE-OFF$
    override protected def show(x: ElemTy): String = x.toString
    override protected def head(xs: ErrorStack): ElemTy = xs.error
    override protected def tail(xs: ErrorStack): ErrorStack = xs.tail
    // $COVERAGE-ON$
}
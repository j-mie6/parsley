/* SPDX-FileCopyrightText: © 2021 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.stacks

private [machine] final class CheckStack(var offset: Int, val tail: CheckStack)
private [machine] object CheckStack extends Stack[CheckStack] {
    implicit val inst: Stack[CheckStack] = this
    type ElemTy = Int
    // $COVERAGE-OFF$
    override protected def show(x: ElemTy): String = x.toString
    override protected def head(xs: CheckStack): ElemTy = xs.offset
    override protected def tail(xs: CheckStack): CheckStack = xs.tail
    // $COVERAGE-ON$
}

/* SPDX-FileCopyrightText: © 2021 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.stacks

import scala.annotation.tailrec

import parsley.internal.machine.instructions.Instr

private [machine] final class CallStack(val ret: Int, val instrs: Array[Instr], val callId: Int, val tail: CallStack)
private [machine] object CallStack extends Stack[CallStack] {
    implicit val inst: Stack[CallStack] = this
    type ElemTy = (Int, Array[Instr])
    // $COVERAGE-OFF$
    override protected def show(x: ElemTy): String = x._1.toString
    override protected def head(xs: CallStack): ElemTy = (xs.ret, xs.instrs)
    override protected def tail(xs: CallStack): CallStack = xs.tail
    // $COVERAGE-ON$
}

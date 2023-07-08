/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.instructions.token

import parsley.internal.machine.Context

private [parsley] sealed abstract class CharPredicate {
    //def pop(ctx: Context): Boolean
    def peek(ctx: Context): Boolean
}
private [parsley] class Basic(f: Char => Boolean) extends CharPredicate {
    def peek(ctx: Context): Boolean = ctx.moreInput && f(ctx.peekChar)
}
private [parsley] class Unicode(f: Int => Boolean) extends CharPredicate {
    def peek(ctx: Context): Boolean = {
        lazy val hc = ctx.peekChar(0)
        lazy val l = ctx.peekChar(1)
        ctx.moreInput(2) && hc.isHighSurrogate && Character.isSurrogatePair(hc, l) && f(Character.toCodePoint(hc, l)) || ctx.moreInput && f(hc.toInt)
    }
}
private [parsley] object NotRequired extends CharPredicate {
    // $COVERAGE-OFF$
    def peek(ctx: Context): Boolean = false
    // $COVERAGE-ON$
}

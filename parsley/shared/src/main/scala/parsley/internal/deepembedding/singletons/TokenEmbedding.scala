/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.singletons

import parsley.token.descriptions.numeric.PlusSignPresence

import parsley.internal.deepembedding.Sign.SignType
import parsley.internal.machine.instructions

private [parsley] final class WhiteSpace(ws: Char => Boolean, start: String, end: String, line: String, nested: Boolean, eofAllowed: Boolean)
    extends Singleton[Unit] {
    // $COVERAGE-OFF$
    override val pretty: String = "whiteSpace"
    override def instr: instructions.Instr = new instructions.TokenWhiteSpace(ws, start, end, line, nested, eofAllowed)
}

private [parsley] final class SkipComments(start: String, end: String, line: String, nested: Boolean, eofAllowed: Boolean) extends Singleton[Unit] {
    // $COVERAGE-OFF$
    override val pretty: String = "skipComments"
    override def instr: instructions.Instr = new instructions.TokenSkipComments(start, end, line, nested, eofAllowed)
}

private [parsley] final class Comment(start: String, end: String, line: String, nested: Boolean, eofAllowed: Boolean) extends Singleton[Unit] {
    // $COVERAGE-OFF$
    override val pretty: String = "comment"
    override def instr: instructions.Instr = new instructions.TokenComment(start, end, line, nested, eofAllowed)
}

private [parsley] final class Sign[A](ty: SignType, signPresence: PlusSignPresence) extends Singleton[A => A] {
    // $COVERAGE-OFF$
    override val pretty: String = "sign"
    override def instr: instructions.Instr = new instructions.TokenSign(ty, signPresence)
}

private [parsley] class NonSpecific(override val pretty: String, name: String, illegalName: String,
                                    start: Char => Boolean, letter: Char => Boolean, illegal: String => Boolean) extends Singleton[String] {
    override def instr: instructions.Instr = new instructions.TokenNonSpecific(name, illegalName)(start, letter, illegal)
}

private [parsley] final class Specific(private [Specific] val specific: String, letter: Char => Boolean, val caseSensitive: Boolean)
    extends Singleton[Unit] {
    // $COVERAGE-OFF$
    override def pretty: String = s"specific($specific)"
    // $COVERAGE-ON$
    override def instr: instructions.Instr = new instructions.TokenSpecific(specific, letter, caseSensitive)
}

/*
private [parsley] final class MaxOp(private [MaxOp] val operator: String, ops: Set[String]) extends Singleton[Unit] {
    // $COVERAGE-OFF$
    override def pretty: String = s"maxOp($operator)"
    // $COVERAGE-ON$
    override def instr: instructions.Instr = new instructions.TokenMaxOp(operator, ops)
}
*/

// $COVERAGE-OFF$
private [deepembedding] object Specific {
    def unapply(self: Specific): Some[String] = Some(self.specific)
}
// $COVERAGE-ON$

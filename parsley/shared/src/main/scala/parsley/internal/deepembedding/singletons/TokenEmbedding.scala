/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.singletons

import parsley.token.descriptions.SpaceDesc
import parsley.token.descriptions.numeric.PlusSignPresence
import parsley.token.errors.{ErrorConfig, LabelConfig}

import parsley.internal.deepembedding.Sign.SignType
import parsley.internal.machine.instructions

private [parsley] final class WhiteSpace(ws: Char => Boolean, desc: SpaceDesc, errConfig: ErrorConfig)
    extends Singleton[Unit] {
    // $COVERAGE-OFF$
    override val pretty: String = "whiteSpace"
    override def instr: instructions.Instr = new instructions.TokenWhiteSpace(ws, desc, errConfig)
}

private [parsley] final class SkipComments(desc: SpaceDesc, errConfig: ErrorConfig) extends Singleton[Unit] {
    // $COVERAGE-OFF$
    override val pretty: String = "skipComments"
    override def instr: instructions.Instr = new instructions.TokenSkipComments(desc, errConfig)
}

private [parsley] final class Comment(desc: SpaceDesc, errConfig: ErrorConfig) extends Singleton[Unit] {
    // $COVERAGE-OFF$
    override val pretty: String = "comment"
    override def instr: instructions.Instr = new instructions.TokenComment(desc, errConfig)
}

private [parsley] final class Sign[A](ty: SignType, signPresence: PlusSignPresence) extends Singleton[A => A] {
    // $COVERAGE-OFF$
    override val pretty: String = "sign"
    override def instr: instructions.Instr = new instructions.TokenSign(ty, signPresence)
}

private [parsley] class NonSpecific(name: String, unexpectedIllegal: String => String,
                                    start: Char => Boolean, letter: Char => Boolean, illegal: String => Boolean) extends Singleton[String] {
    // $COVERAGE-OFF$
    override def pretty: String = "nonspecificName"
    // $COVERAGE-ON$
    override def instr: instructions.Instr = new instructions.TokenNonSpecific(name, unexpectedIllegal)(start, letter, illegal)
}

private [parsley] final class Specific(private [Specific] val specific: String, expected: LabelConfig,
                                       expectedEnd: String, letter: Char => Boolean, val caseSensitive: Boolean) extends Singleton[Unit] {
    // $COVERAGE-OFF$
    override def pretty: String = s"specific($specific)"
    // $COVERAGE-ON$
    override def instr: instructions.Instr = new instructions.TokenSpecific(specific, expected, expectedEnd, letter, caseSensitive)
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

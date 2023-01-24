/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.singletons

import parsley.registers.Reg
import parsley.token.errors.LabelConfig

import parsley.internal.deepembedding.frontend.UsesRegister
import parsley.internal.machine.instructions

private [parsley] final class CharTok(private [CharTok] val c: Char, val expected: LabelConfig) extends Singleton[Char] {
    // $COVERAGE-OFF$
    override def pretty: String = s"char($c)"
    // $COVERAGE-ON$
    override def instr: instructions.Instr = instructions.CharTok(c, expected)
}

private [parsley] final class StringTok(private [StringTok] val s: String, val expected: LabelConfig) extends Singleton[String] {
    // $COVERAGE-OFF$
    override def pretty: String = s"string($s)"
    // $COVERAGE-ON$
    override def instr: instructions.Instr = instructions.StringTok(s, expected)
}

private [parsley] object Eof extends Singleton[Unit] {
    // $COVERAGE-OFF$
    override val pretty: String = "eof"
    // $COVERAGE-ON$
    override val instr: instructions.Instr = instructions.Eof
}

private [parsley] final class UniSatisfy(private [UniSatisfy] val f: Int => Boolean, val expected: LabelConfig) extends Singleton[Int] {
    // $COVERAGE-OFF$
    override def pretty: String = "satisfyUnicode(?)"
    // $COVERAGE-ON$
    override def instr: instructions.Instr = new instructions.UniSat(f, expected)
}

private [parsley] final class Modify[S](val reg: Reg[S], f: S => S) extends Singleton[Unit] with UsesRegister {
    // $COVERAGE-OFF$
    override def pretty: String = s"modify($reg, ?)"
    // $COVERAGE-ON$
    override def instr: instructions.Instr = instructions.Modify(reg.addr, f)
}

private [deepembedding] object CharTok {
    def unapply(self: CharTok): Option[Char] = Some(self.c)
}
private [deepembedding] object StringTok {
    def unapply(self: StringTok): Option[String] = Some(self.s)
}
private [deepembedding] object UniSatisfy {
    def unapply(self: UniSatisfy): Option[Int => Boolean] = Some(self.f)
}

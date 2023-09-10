/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.singletons

import parsley.registers.Reg
import parsley.token.errors.LabelConfig

import parsley.internal.deepembedding.backend.StrictParsley.InstrBuffer
import parsley.internal.deepembedding.frontend.{LazyParsleyIVisitor, UsesRegister}
import parsley.internal.machine.instructions

private [parsley] final class CharTok(private val c: Char, val expected: LabelConfig) extends Singleton[Char] {
    // $COVERAGE-OFF$
    override def pretty: String = s"char($c)"
    // $COVERAGE-ON$
    override def genInstrs(producesResults: Boolean)(implicit instrs: InstrBuffer): Unit = {
        instrs += new instructions.CharTok(c, expected)
        if (producesResults) instrs += new instructions.Push(c)
    }

    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[Char] = visitor.visit(this, context)(c, expected)
}

private [parsley] final class SupplementaryCharTok(private val codepoint: Int, val expected: LabelConfig) extends Singleton[Int] {
    // $COVERAGE-OFF$
    override def pretty: String = s"char(${Character.toChars(codepoint).mkString})"
    // $COVERAGE-ON$
    override def genInstrs(producesResults: Boolean)(implicit instrs: InstrBuffer): Unit = {
        instrs += new instructions.SupplementaryCharTok(codepoint, expected)
        if (producesResults) instrs += new instructions.Push(codepoint)
    }

    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[Int] = visitor.visit(this, context)(codepoint, expected)
}

private [parsley] final class StringTok(private val s: String, val expected: LabelConfig) extends Singleton[String] {
    // $COVERAGE-OFF$
    override def pretty: String = s"string($s)"
    // $COVERAGE-ON$
    override def genInstrs(producesResults: Boolean)(implicit instrs: InstrBuffer): Unit = {
        instrs += new instructions.StringTok(s, expected)
        if (producesResults) instrs += new instructions.Push(s)
    }

    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[String] = visitor.visit(this, context)(s, expected)
}

private [parsley] object Eof extends Singleton[Unit] {
    // $COVERAGE-OFF$
    override def pretty: String = "eof"
    // $COVERAGE-ON$
    override def genInstrs(producesResults: Boolean)(implicit instrs: InstrBuffer): Unit = {
        instrs += instructions.Eof
        if (producesResults) instrs += instructions.Push.Unit
    }

    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[Unit] = visitor.visit(this, context)
}

private [parsley] final class UniSatisfy(private [UniSatisfy] val f: Int => Boolean, val expected: LabelConfig) extends Singleton[Int] {
    // $COVERAGE-OFF$
    override def pretty: String = "satisfyUnicode(?)"
    // $COVERAGE-ON$
    override def genInstrs(producesResults: Boolean)(implicit instrs: InstrBuffer): Unit = {
        instrs += new instructions.UniSat(f, expected)
        if (!producesResults) instrs += instructions.Pop
    }

    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[Int] = visitor.visit(this, context)(f, expected)
}

private [parsley] final class Modify[S](val reg: Reg[S], f: S => S) extends Singleton[Unit] with UsesRegister {
    // $COVERAGE-OFF$
    override def pretty: String = s"modify($reg, ?)"
    // $COVERAGE-ON$
    override def genInstrs(producesResults: Boolean)(implicit instrs: InstrBuffer): Unit = {
        instrs += instructions.Modify(reg.addr, f)
        if (producesResults) instrs += instructions.Push.Unit
    }

    override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[Unit] = visitor.visit(this, context)(reg, f)
}

private [deepembedding] object CharTok {
    def unapply(self: CharTok): Some[Char] = Some(self.c)
}
private [deepembedding] object SupplementaryCharTok {
    def unapply(self: SupplementaryCharTok): Some[Int] = Some(self.codepoint)
}
private [deepembedding] object StringTok {
    def unapply(self: StringTok): Some[String] = Some(self.s)
}
private [deepembedding] object UniSatisfy {
    def unapply(self: UniSatisfy): Some[Int => Boolean] = Some(self.f)
}

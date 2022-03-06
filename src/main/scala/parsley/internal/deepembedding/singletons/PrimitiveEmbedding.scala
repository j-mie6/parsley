package parsley.internal.deepembedding.singletons

import parsley.internal.machine.instructions
import parsley.registers.Reg

private [parsley] final class Satisfy(private [Satisfy] val f: Char => Boolean, val expected: Option[String]) extends Singleton[Char] {
    // $COVERAGE-OFF$
    override val pretty = "satisfy(f)"
    // $COVERAGE-ON$
    override def instr = new instructions.Satisfies(f, expected)
}

private [parsley] object Line extends Singleton[Int] {
    // $COVERAGE-OFF$
    override val pretty = "line"
    // $COVERAGE-ON$
    override val instr = instructions.Line
}
private [parsley] object Col extends Singleton[Int] {
    // $COVERAGE-OFF$
    override val pretty = "col"
    // $COVERAGE-ON$
    override val instr = instructions.Col
}
private [parsley] final class Get[S](reg: Reg[S]) extends Singleton[S] {
    // $COVERAGE-OFF$
    override def pretty = s"get($reg)"
    // $COVERAGE-ON$
    override def instr = new instructions.Get(reg.addr)
}

private [deepembedding] object Satisfy {
    def unapply(self: Satisfy): Some[Char => Boolean] = Some(self.f)
}
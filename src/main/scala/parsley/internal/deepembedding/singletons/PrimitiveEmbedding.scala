package parsley.internal.deepembedding.singletons

import parsley.internal.machine.instructions
import parsley.registers.Reg

private [parsley] final class Satisfy(private [Satisfy] val f: Char => Boolean, val expected: Option[String]) extends Singleton[Char] {
    override val pretty = "satisfy(f)"
    override def instr = new instructions.Satisfies(f, expected)
}

private [parsley] object Line extends Singleton[Int] {
    override val pretty = "line"
    override val instr = instructions.Line
}
private [parsley] object Col extends Singleton[Int] {
    override val pretty = "col"
    override val instr = instructions.Col
}
private [parsley] final class Get[S](reg: Reg[S]) extends Singleton[S] {
    override def pretty = s"get($reg)"
    override def instr = new instructions.Get(reg.addr)
}

private [deepembedding] object Satisfy {
    def unapply(self: Satisfy): Some[Char => Boolean] = Some(self.f)
}
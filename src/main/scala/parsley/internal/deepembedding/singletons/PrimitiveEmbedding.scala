package parsley.internal.deepembedding.singletons

import parsley.internal.machine.instructions
import parsley.registers.Reg

private [parsley] final class Satisfy(private [Satisfy] val f: Char => Boolean, val expected: Option[String])
    extends Singleton[Char]("satisfy(f)", new instructions.Satisfies(f, expected))

private [parsley] object Line extends Singleton[Int]("line", instructions.Line)
private [parsley] object Col extends Singleton[Int]("col", instructions.Col)
private [parsley] final class Get[S](reg: Reg[S]) extends Singleton[S](s"get($reg)", new instructions.Get(reg.addr))

private [deepembedding] object Satisfy {
    def unapply(self: Satisfy): Some[Char => Boolean] = Some(self.f)
}
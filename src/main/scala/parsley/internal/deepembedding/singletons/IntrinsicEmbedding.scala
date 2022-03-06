package parsley.internal.deepembedding.singletons

import parsley.internal.machine.instructions
import parsley.internal.deepembedding.backend.StrictParsley
import parsley.internal.deepembedding.frontend.UsesRegister
import parsley.registers.Reg

private [parsley] final class CharTok(private [CharTok] val c: Char, val expected: Option[String])
    extends Singleton[Char](s"char($c)", instructions.CharTok(c, expected))

private [parsley] final class StringTok(private [StringTok] val s: String, val expected: Option[String])
    extends Singleton[String](s"string($s)", instructions.StringTok(s, expected)) {
    override def optimise: StrictParsley[String] = s match {
        case "" => new Pure("")
        case _ => this
    }
}

private [parsley] object Eof extends Singleton[Unit]("eof", instructions.Eof)

private [parsley] final class Modify[S](val reg: Reg[S], f: S => S) extends Singleton[Unit](s"modify($reg, ?)", new instructions.Modify(reg.addr, f)) with UsesRegister

private [deepembedding] object CharTok {
    def unapply(self: CharTok): Option[Char] = Some(self.c)
}
private [deepembedding] object StringTok {
    def unapply(self: StringTok): Option[String] = Some(self.s)
}
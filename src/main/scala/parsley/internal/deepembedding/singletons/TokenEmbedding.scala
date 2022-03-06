package parsley.internal.deepembedding.singletons

import parsley.internal.deepembedding.Sign.SignType
import parsley.internal.machine.instructions

private [parsley] final class WhiteSpace(ws: Char => Boolean, start: String, end: String, line: String, nested: Boolean) extends Singleton[Unit] {
    override val pretty = "whiteSpace"
    override def instr = new instructions.TokenWhiteSpace(ws, start, end, line, nested)
}

private [parsley] final class SkipComments(start: String, end: String, line: String, nested: Boolean) extends Singleton[Unit] {
    override val pretty = "skipComments"
    override def instr = new instructions.TokenSkipComments(start, end, line, nested)
}

private [parsley] final class Comment(start: String, end: String, line: String, nested: Boolean) extends Singleton[Unit] {
    override val pretty = "comment"
    override def instr = new instructions.TokenComment(start, end, line, nested)
}

private [parsley] final class Sign[A](ty: SignType) extends Singleton[A => A] {
    override val pretty = "sign"
    override def instr = new instructions.TokenSign(ty)
}

private [parsley] object Natural extends Singleton[Int] {
    override val pretty = "natural"
    override val instr = instructions.TokenNatural
}

private [parsley] object Float extends Singleton[Double] {
    override val pretty = "float"
    override val instr = instructions.TokenFloat
}

private [parsley] object Escape extends Singleton[Char] {
    override val pretty = "escape"
    override def instr = new instructions.TokenEscape
}

private [parsley] final class StringLiteral(ws: Char => Boolean) extends Singleton[String] {
    override val pretty = "stringLiteral"
    override def instr = new instructions.TokenString(ws)
}

private [parsley] object RawStringLiteral extends Singleton[String] {
    override val pretty = "rawStringLiteral"
    override val instr = instructions.TokenRawString
}

private [parsley] class NonSpecific(override val pretty: String, name: String, illegalName: String,
                                    start: Char => Boolean, letter: Char => Boolean, illegal: String => Boolean) extends Singleton[String] {
    override def instr = new instructions.TokenNonSpecific(name, illegalName)(start, letter, illegal)
}

private [parsley] final class Specific(name: String, private [Specific] val specific: String, letter: Char => Boolean, val caseSensitive: Boolean)
    extends Singleton[Unit] {
    override def pretty = s"$name($specific)"
    override def instr = new instructions.TokenSpecific(specific, letter, caseSensitive)
}

private [parsley] final class MaxOp(private [MaxOp] val operator: String, ops: Set[String]) extends Singleton[Unit] {
    override def pretty = s"maxOp($operator)"
    override def instr = new instructions.TokenMaxOp(operator, ops)
}

// $COVERAGE-OFF$
private [deepembedding] object Specific {
    def unapply(self: Specific): Some[String] = Some(self.specific)
}
private [deepembedding] object MaxOp {
    def unapply(self: MaxOp): Some[String] = Some(self.operator)
}
// $COVERAGE-ON$
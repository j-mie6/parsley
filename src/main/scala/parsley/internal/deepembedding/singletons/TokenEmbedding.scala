package parsley.internal.deepembedding.singletons

import parsley.internal.deepembedding.Sign.SignType
import parsley.internal.machine.instructions

private [parsley] final class WhiteSpace(ws: Char => Boolean, start: String, end: String, line: String, nested: Boolean) extends Singleton[Unit] {
    // $COVERAGE-OFF$
    override val pretty: String = "whiteSpace"
    override def instr: instructions.Instr = new instructions.TokenWhiteSpace(ws, start, end, line, nested)
}

private [parsley] final class SkipComments(start: String, end: String, line: String, nested: Boolean) extends Singleton[Unit] {
    // $COVERAGE-OFF$
    override val pretty: String = "skipComments"
    override def instr: instructions.Instr = new instructions.TokenSkipComments(start, end, line, nested)
}

private [parsley] final class Comment(start: String, end: String, line: String, nested: Boolean) extends Singleton[Unit] {
    // $COVERAGE-OFF$
    override val pretty: String = "comment"
    override def instr: instructions.Instr = new instructions.TokenComment(start, end, line, nested)
}

private [parsley] final class Sign[A](ty: SignType) extends Singleton[A => A] {
    // $COVERAGE-OFF$
    override val pretty: String = "sign"
    override def instr: instructions.Instr = new instructions.TokenSign(ty)
}

private [parsley] object Natural extends Singleton[Int] {
    // $COVERAGE-OFF$
    override val pretty: String = "natural"
    override val instr: instructions.Instr = instructions.TokenNatural
}

private [parsley] object Float extends Singleton[Double] {
    // $COVERAGE-OFF$
    override val pretty: String = "float"
    override val instr: instructions.Instr = instructions.TokenFloat
}

private [parsley] object Escape extends Singleton[Char] {
    // $COVERAGE-OFF$
    override val pretty: String = "escape"
    override def instr: instructions.Instr = new instructions.TokenEscape
}

private [parsley] final class StringLiteral(ws: Char => Boolean) extends Singleton[String] {
    // $COVERAGE-OFF$
    override val pretty: String = "stringLiteral"
    override def instr: instructions.Instr = new instructions.TokenString(ws)
}

private [parsley] object RawStringLiteral extends Singleton[String] {
    // $COVERAGE-OFF$
    override val pretty: String = "rawStringLiteral"
    override val instr: instructions.Instr = instructions.TokenRawString
}

private [parsley] class NonSpecific(override val pretty: String, name: String, illegalName: String,
                                    start: Char => Boolean, letter: Char => Boolean, illegal: String => Boolean) extends Singleton[String] {
    override def instr: instructions.Instr = new instructions.TokenNonSpecific(name, illegalName)(start, letter, illegal)
}

private [parsley] final class Specific(name: String, private [Specific] val specific: String, letter: Char => Boolean, val caseSensitive: Boolean)
    extends Singleton[Unit] {
    // $COVERAGE-OFF$
    override def pretty: String = s"$name($specific)"
    // $COVERAGE-ON$
    override def instr: instructions.Instr = new instructions.TokenSpecific(specific, letter, caseSensitive)
}

private [parsley] final class MaxOp(private [MaxOp] val operator: String, ops: Set[String]) extends Singleton[Unit] {
    // $COVERAGE-OFF$
    override def pretty: String = s"maxOp($operator)"
    // $COVERAGE-ON$
    override def instr: instructions.Instr = new instructions.TokenMaxOp(operator, ops)
}

// $COVERAGE-OFF$
private [deepembedding] object Specific {
    def unapply(self: Specific): Some[String] = Some(self.specific)
}
private [deepembedding] object MaxOp {
    def unapply(self: MaxOp): Some[String] = Some(self.operator)
}
// $COVERAGE-ON$
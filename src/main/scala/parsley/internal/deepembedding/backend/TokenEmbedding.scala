package parsley.internal.deepembedding.backend

import parsley.internal.deepembedding.Sign.SignType
import parsley.internal.machine.instructions

private [deepembedding] final class WhiteSpace(ws: Char => Boolean, start: String, end: String, line: String, nested: Boolean)
    extends Singleton[Unit](new instructions.TokenWhiteSpace(ws, start, end, line, nested))

private [deepembedding] final class SkipComments(start: String, end: String, line: String, nested: Boolean)
    extends Singleton[Unit](new instructions.TokenSkipComments(start, end, line, nested))

private [deepembedding] final class Comment(start: String, end: String, line: String, nested: Boolean)
    extends Singleton[Unit](new instructions.TokenComment(start, end, line, nested))

private [deepembedding] final class Sign[A](ty: SignType) extends Singleton[A => A](new instructions.TokenSign(ty))

private [deepembedding] object Natural extends Singleton[Int](instructions.TokenNatural)

private [deepembedding] object Float extends Singleton[Double](instructions.TokenFloat)

private [deepembedding] object Escape extends Singleton[Char](new instructions.TokenEscape)

private [deepembedding] final class StringLiteral(ws: Char => Boolean) extends Singleton[String](new instructions.TokenString(ws))

private [deepembedding] object RawStringLiteral extends Singleton[String](instructions.TokenRawString)

private [deepembedding] class NonSpecific(name: String, illegalName: String, start: Char => Boolean, letter: Char => Boolean, illegal: String => Boolean)
    extends Singleton[String](new instructions.TokenNonSpecific(name, illegalName)(start, letter, illegal))

private [deepembedding] final class Specific(name: String, private [Specific] val specific: String, letter: Char => Boolean, val caseSensitive: Boolean)
    extends Singleton[Unit](new instructions.TokenSpecific(specific, letter, caseSensitive))

private [deepembedding] final class MaxOp(private [MaxOp] val operator: String, ops: Set[String])
    extends Singleton[Unit](new instructions.TokenMaxOp(operator, ops))

private [deepembedding] object Sign {
    private [deepembedding] sealed trait SignType {
        type resultType
    }
    private [deepembedding] case object DoubleType extends SignType {
        override type resultType = Double
    }
    private [deepembedding] case object IntType extends SignType {
        override type resultType = Int
    }
}

// $COVERAGE-OFF$
private [backend] object Specific {
    def unapply(self: Specific): Some[String] = Some(self.specific)
}
private [backend] object MaxOp {
    def unapply(self: MaxOp): Some[String] = Some(self.operator)
}
// $COVERAGE-ON$
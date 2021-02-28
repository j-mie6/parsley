package parsley.internal.deepembedding

import parsley.token.TokenSet
import Sign.SignType
import parsley.internal.machine.instructions

private [parsley] final class WhiteSpace(ws: TokenSet, start: String, end: String, line: String, nested: Boolean)
    extends Singleton[Unit]("whiteSpace", new instructions.TokenWhiteSpace(ws, start, end, line, nested))

private [parsley] final class SkipComments(start: String, end: String, line: String, nested: Boolean)
    extends Singleton[Unit]("skipComments", new instructions.TokenSkipComments(start, end, line, nested))

private [parsley] final class Comment(start: String, end: String, line: String, nested: Boolean)
    extends Singleton[Unit]("comment", new instructions.TokenComment(start, end, line, nested))

private [parsley] final class Sign[A](ty: SignType)
    extends SingletonExpect[A => A]("sign", _ => new Sign(ty), new instructions.TokenSign(ty))

private [parsley] final class Natural(val expected: Option[String] = None)
    extends SingletonExpect[Int]("natural", new Natural(_), new instructions.TokenNatural(expected))

private [parsley] final class Float(val expected: Option[String] = None)
    extends SingletonExpect[Double]("float", new Float(_), new instructions.TokenFloat(expected))

private [parsley] final class Escape(val expected: Option[String] = None)
    extends SingletonExpect[Char]("escape", new Escape(_), new instructions.TokenEscape(expected))

private [parsley] final class StringLiteral(ws: TokenSet, val expected: Option[String] = None)
    extends SingletonExpect[String]("stringLiteral", new StringLiteral(ws, _), new instructions.TokenString(ws, expected))

private [parsley] final class RawStringLiteral(val expected: Option[String] = None)
    extends SingletonExpect[String]("rawStringLiteral", new RawStringLiteral(_), new instructions.TokenRawString(expected))

private [parsley] class NonSpecific(combinatorName: String, name: String, illegalName: String, start: TokenSet,
                                    letter: TokenSet, illegal: String => Boolean, val expected: Option[String] = None)
    extends SingletonExpect[String](combinatorName, new NonSpecific(combinatorName, name, illegalName, start, letter, illegal, _),
                                    new instructions.TokenNonSpecific(name, illegalName)(start, letter, illegal, expected))

private [parsley] final class Specific(name: String, private [Specific] val specific: String,
                                       letter: TokenSet, caseSensitive: Boolean, val expected: Option[String] = None)
    extends SingletonExpect[Unit](s"$name($specific)", new Specific(name, specific, letter, caseSensitive, _),
                                  new instructions.TokenSpecific(specific, letter, caseSensitive, expected))

private [parsley] final class MaxOp(private [MaxOp] val operator: String, ops: Set[String], val expected: Option[String] = None)
    extends SingletonExpect[Unit](s"maxOp($operator)", new MaxOp(operator, ops, _), new instructions.TokenMaxOp(operator, ops, expected))

private [parsley] object Sign {
    private [parsley] sealed trait SignType {
        type resultType
    }
    private [parsley] case object DoubleType extends SignType {
        override type resultType = Double
    }
    private [parsley] case object IntType extends SignType {
        override type resultType = Int
    }
}

// $COVERAGE-OFF$
private [deepembedding] object Specific {
    def unapply(self: Specific): Option[String] = Some(self.specific)
}
private [deepembedding] object MaxOp {
    def unapply(self: MaxOp): Option[String] = Some(self.operator)
}
// $COVERAGE-ON$
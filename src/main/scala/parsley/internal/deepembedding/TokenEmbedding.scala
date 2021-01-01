package parsley.internal.deepembedding

import parsley.TokenParser.TokenSet
import Sign.SignType
import parsley.internal.{instructions, UnsafeOption}

private [parsley] class WhiteSpace(ws: TokenSet, start: String, end: String, line: String, nested: Boolean)
    extends Singleton[Unit]("whiteSpace", new instructions.TokenWhiteSpace(ws, start, end, line, nested))

private [parsley] class SkipComments(start: String, end: String, line: String, nested: Boolean)
    extends Singleton[Unit]("skipComments", new instructions.TokenSkipComments(start, end, line, nested))

private [parsley] class Comment(start: String, end: String, line: String, nested: Boolean)
    extends Singleton[Unit]("comment", new instructions.TokenComment(start, end, line, nested))

private [parsley] class Sign[A](ty: SignType, val expected: UnsafeOption[String] = null)
    extends SingletonExpect[A => A]("sign", new Sign(ty, _), new instructions.TokenSign(ty, expected))

private [parsley] class Natural(val expected: UnsafeOption[String] = null)
    extends SingletonExpect[Int]("natural", new Natural(_), new instructions.TokenNatural(expected))

private [parsley] class Float(val expected: UnsafeOption[String] = null)
    extends SingletonExpect[Double]("float", new Float(_), new instructions.TokenFloat(expected))

private [parsley] class Escape(val expected: UnsafeOption[String] = null)
    extends SingletonExpect[Char]("escape", new Escape(_), new instructions.TokenEscape(expected))

private [parsley] class StringLiteral(ws: TokenSet, val expected: UnsafeOption[String] = null)
    extends SingletonExpect[String]("stringLiteral", new StringLiteral(ws, _), new instructions.TokenString(ws, expected))

private [parsley] class RawStringLiteral(val expected: UnsafeOption[String] = null)
    extends SingletonExpect[String]("rawStringLiteral", new RawStringLiteral(_), new instructions.TokenRawString(expected))

private [parsley] class Identifier(start: TokenSet, letter: TokenSet, keywords: Set[String], val expected: UnsafeOption[String] = null)
    extends SingletonExpect[String]("identifier", new Identifier(start, letter, keywords, _),
                                    new instructions.TokenIdentifier(start, letter, keywords, expected))

private [parsley] class UserOp(start: TokenSet, letter: TokenSet, ops: Set[String], val expected: UnsafeOption[String] = null)
    extends SingletonExpect[String]("userOp", new UserOp(start, letter, ops, _), new instructions.TokenUserOperator(start, letter, ops, expected))

private [parsley] class ReservedOp(start: TokenSet, letter: TokenSet, ops: Set[String], val expected: UnsafeOption[String] = null)
    extends SingletonExpect[String]("reservedOp", new ReservedOp(start, letter, ops, _), new instructions.TokenOperator(start, letter, ops, expected))

private [parsley] class Keyword(private [Keyword] val keyword: String, letter: TokenSet, caseSensitive: Boolean, val expected: UnsafeOption[String] = null)
    extends SingletonExpect[Unit](s"keyword($keyword)", new Keyword(keyword, letter, caseSensitive, _),
                                  new instructions.TokenKeyword(keyword, letter, caseSensitive, expected))

private [parsley] class Operator(private [Operator] val operator: String, letter: TokenSet, val expected: UnsafeOption[String] = null)
    extends SingletonExpect[Unit](s"operator($operator)", new Operator(operator, letter, _), new instructions.TokenOperator_(operator, letter, expected))

private [parsley] class MaxOp(private [MaxOp] val operator: String, ops: Set[String], val expected: UnsafeOption[String] = null)
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
private [deepembedding] object Keyword {
    def unapply(self: Keyword): Option[String] = Some(self.keyword)
}
private [deepembedding] object Operator {
    def unapply(self: Operator): Option[String] = Some(self.operator)
}
private [deepembedding] object MaxOp {
    def unapply(self: MaxOp): Option[String] = Some(self.operator)
}
// $COVERAGE-ON$
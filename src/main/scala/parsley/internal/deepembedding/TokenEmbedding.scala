package parsley.internal.deepembedding

import Sign.SignType

private [parsley] final class WhiteSpace(ws: Char => Boolean, start: String, end: String, line: String, nested: Boolean)
    extends Singleton[Unit]("whiteSpace", new backend.WhiteSpace(ws, start, end, line, nested))

private [parsley] final class SkipComments(start: String, end: String, line: String, nested: Boolean)
    extends Singleton[Unit]("skipComments", new backend.SkipComments(start, end, line, nested))

private [parsley] final class Comment(start: String, end: String, line: String, nested: Boolean)
    extends Singleton[Unit]("comment", new backend.Comment(start, end, line, nested))

private [parsley] final class Sign[A](ty: SignType)
    extends Singleton[A => A]("sign", new backend.Sign(ty))

private [parsley] object Natural
    extends Singleton[Int]("natural", backend.Natural)

private [parsley] object Float
    extends Singleton[Double]("float", backend.Float)

private [parsley] object Escape
    extends Singleton[Char]("escape", backend.Escape)

private [parsley] final class StringLiteral(ws: Char => Boolean)
    extends Singleton[String]("stringLiteral", new backend.StringLiteral(ws))

private [parsley] object RawStringLiteral
    extends Singleton[String]("rawStringLiteral", backend.RawStringLiteral)

private [parsley] class NonSpecific(combinatorName: String, name: String, illegalName: String, start: Char => Boolean,
                                    letter: Char => Boolean, illegal: String => Boolean)
    extends Singleton[String](combinatorName, new backend.NonSpecific(name, illegalName, start, letter, illegal))

private [parsley] final class Specific(name: String, private [Specific] val specific: String,
                                       letter: Char => Boolean, val caseSensitive: Boolean)
    extends Singleton[Unit](s"$name($specific)", new backend.Specific(name, specific, letter, caseSensitive))

private [parsley] final class MaxOp(private [MaxOp] val operator: String, ops: Set[String])
    extends Singleton[Unit](s"maxOp($operator)", new backend.MaxOp(operator, ops))

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
package parsley.token.errors

import parsley.Parsley, Parsley.{pure, empty}
import parsley.character.satisfyUtf16
import parsley.errors.combinator, combinator.ErrorMethods
import parsley.position

sealed abstract class PreventDotIsZeroConfig {
    private [parsley] def apply(p: Parsley[Boolean]): Parsley[Boolean]
}
final case class UnexpectedZeroDot(unexpected: String) extends PreventDotIsZeroConfig {
    private [parsley] override def apply(p: Parsley[Boolean]): Parsley[Boolean] = p.unexpectedWhen { case true => unexpected }
}
final case class UnexpectedZeroDotWithReason(unexpected: String, reason: String) extends PreventDotIsZeroConfig {
    private [parsley] override def apply(p: Parsley[Boolean]): Parsley[Boolean] = combinator.amendThenDislodge {
        position.internalOffsetSpan(combinator.entrench(p)).flatMap { case (os, x, oe) =>
            if (x) combinator.unexpected(oe - os, unexpected).explain(reason)
            else pure(x)
        }
    }
}
final case class ZeroDotReason(reason: String) extends PreventDotIsZeroConfig {
    private [parsley] override def apply(p: Parsley[Boolean]): Parsley[Boolean] = p.filterOut { case true => reason }
}
final case class ZeroDotFail(msg0: String, msgs: String*) extends PreventDotIsZeroConfig {
    private [parsley] override def apply(p: Parsley[Boolean]): Parsley[Boolean] = p.guardAgainst { case true => msg0 +: msgs }
}

sealed abstract class VerifiedBadChars {
    def checkBadChar: Parsley[Nothing]
}
final case class BadCharsFail(cs: Map[Int, Seq[String]]) extends VerifiedBadChars {
    def checkBadChar: Parsley[Nothing] = satisfyUtf16(cs.contains).fail(cs.apply(_))
}
object BadCharsFail {
    def apply(cs: Map[Int, Seq[String]]): VerifiedBadChars = if (cs.isEmpty) Unverified else new BadCharsFail(cs)
}
final case class BadCharsReason(cs: Map[Int, String]) extends VerifiedBadChars {
    def checkBadChar: Parsley[Nothing] = satisfyUtf16(cs.contains)._unexpected(cs.apply)
}
object BadCharsReason {
    def apply(cs: Map[Int, String]): VerifiedBadChars = if (cs.isEmpty) Unverified else new BadCharsReason(cs)
}
case object Unverified extends VerifiedBadChars {
    def checkBadChar: Parsley[Nothing] = empty
}

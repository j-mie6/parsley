package parsley.token.errors

import parsley.Parsley, Parsley.{pure, empty}
import parsley.character.satisfyUtf16
import parsley.errors.combinator, combinator.ErrorMethods
import parsley.position

/**
  * @since 4.1.0
  * @group doubledot
  */
sealed abstract class PreventDotIsZeroConfig {
    private [parsley] def apply(p: Parsley[Boolean]): Parsley[Boolean]
}
/**
  * @since 4.1.0
  * @group doubledot
  */
final class UnexpectedZeroDot private (unexpected: String) extends PreventDotIsZeroConfig {
    private [parsley] override def apply(p: Parsley[Boolean]): Parsley[Boolean] = p.unexpectedWhen { case true => unexpected }
}
/**
  * @since 4.1.0
  * @group doubledot
  */
object UnexpectedZeroDot {
    def apply(unexpected: String): PreventDotIsZeroConfig = new UnexpectedZeroDot(unexpected)
}
/**
  * @since 4.1.0
  * @group doubledot
  */
final class UnexpectedZeroDotWithReason private (unexpected: String, reason: String) extends PreventDotIsZeroConfig {
    private [parsley] override def apply(p: Parsley[Boolean]): Parsley[Boolean] = combinator.amendThenDislodge {
        position.internalOffsetSpan(combinator.entrench(p)).flatMap { case (os, x, oe) =>
            if (x) combinator.unexpected(oe - os, unexpected).explain(reason)
            else pure(x)
        }
    }
}
/**
  * @since 4.1.0
  * @group doubledot
  */
object UnexpectedZeroDotWithReason {
    def apply(unexpected: String, reason: String): PreventDotIsZeroConfig = new UnexpectedZeroDotWithReason(unexpected, reason)
}
/**
  * @since 4.1.0
  * @group doubledot
  */
final class ZeroDotReason private (reason: String) extends PreventDotIsZeroConfig {
    private [parsley] override def apply(p: Parsley[Boolean]): Parsley[Boolean] = p.filterOut { case true => reason }
}
/**
  * @since 4.1.0
  * @group doubledot
  */
object ZeroDotReason {
    def apply(reason: String): PreventDotIsZeroConfig = new ZeroDotReason(reason)
}
/**
  * @since 4.1.0
  * @group doubledot
  */
final class ZeroDotFail private (msg0: String, msgs: String*) extends PreventDotIsZeroConfig {
    private [parsley] override def apply(p: Parsley[Boolean]): Parsley[Boolean] = p.guardAgainst { case true => msg0 +: msgs }
}
/**
  * @since 4.1.0
  * @group doubledot
  */
object ZeroDotFail {
    def apply(msg0: String, msgs: String*): PreventDotIsZeroConfig = new ZeroDotFail(msg0, msgs: _*)
}

/**
  * @since 4.1.0
  * @group badchar
  */
sealed abstract class VerifiedBadChars {
    def checkBadChar: Parsley[Nothing]
}
/**
  * @since 4.1.0
  * @group badchar
  */
final class BadCharsFail private (cs: Map[Int, Seq[String]]) extends VerifiedBadChars {
    def checkBadChar: Parsley[Nothing] = satisfyUtf16(cs.contains).fail(cs.apply(_))
}
/**
  * @since 4.1.0
  * @group badchar
  */
object BadCharsFail {
    def apply(cs: Map[Int, Seq[String]]): VerifiedBadChars = if (cs.isEmpty) Unverified else new BadCharsFail(cs)
}
/**
  * @since 4.1.0
  * @group badchar
  */
final class BadCharsReason private (cs: Map[Int, String]) extends VerifiedBadChars {
    def checkBadChar: Parsley[Nothing] = satisfyUtf16(cs.contains)._unexpected(cs.apply)
}
/**
  * @since 4.1.0
  * @group badchar
  */
object BadCharsReason {
    def apply(cs: Map[Int, String]): VerifiedBadChars = if (cs.isEmpty) Unverified else new BadCharsReason(cs)
}
/**
  * @since 4.1.0
  * @group badchar
  */
object Unverified extends VerifiedBadChars {
    def checkBadChar: Parsley[Nothing] = empty
}

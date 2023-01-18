package parsley.token.errors

import parsley.Parsley, Parsley.{pure, empty}
import parsley.character.satisfyUtf16
import parsley.errors.combinator, combinator.ErrorMethods
import parsley.position

/** This class, and its subclasses, is used to configure what error is generated when `.` is parsed as a real number.
  * @since 4.1.0
  * @group doubledot
  */
sealed abstract class PreventDotIsZeroConfig {
    private [token] def apply(p: Parsley[Boolean]): Parsley[Boolean]
}
private final class UnexpectedZeroDot private (unexpected: String) extends PreventDotIsZeroConfig {
    private [token] override def apply(p: Parsley[Boolean]): Parsley[Boolean] = p.unexpectedWhen { case true => unexpected }
}
/** This object makes "dot is zero" generate a given unexpected message in a ''vanilla'' error.
  * @since 4.1.0
  * @group doubledot
  */
object UnexpectedZeroDot {
    def apply(unexpected: String): PreventDotIsZeroConfig = new UnexpectedZeroDot(unexpected)
}

private final class UnexpectedZeroDotWithReason private (unexpected: String, reason: String) extends PreventDotIsZeroConfig {
    private [token] override def apply(p: Parsley[Boolean]): Parsley[Boolean] = combinator.amendThenDislodge {
        position.internalOffsetSpan(combinator.entrench(p)).flatMap { case (os, x, oe) =>
            if (x) combinator.unexpected(oe - os, unexpected).explain(reason)
            else pure(x)
        }
    }
}
/** This object makes "dot is zero" generate a given unexpected message with a given reason in a ''vanilla'' error.
  * @since 4.1.0
  * @group doubledot
  */
object UnexpectedZeroDotWithReason {
    def apply(unexpected: String, reason: String): PreventDotIsZeroConfig = new UnexpectedZeroDotWithReason(unexpected, reason)
}

private final class ZeroDotReason private (reason: String) extends PreventDotIsZeroConfig {
    private [token] override def apply(p: Parsley[Boolean]): Parsley[Boolean] = p.filterOut { case true => reason }
}
/** This object makes "dot is zero" generate a given reason in a ''vanilla'' error.
  * @since 4.1.0
  * @group doubledot
  */
object ZeroDotReason {
    def apply(reason: String): PreventDotIsZeroConfig = new ZeroDotReason(reason)
}

private final class ZeroDotFail private (msg0: String, msgs: String*) extends PreventDotIsZeroConfig {
    private [token] override def apply(p: Parsley[Boolean]): Parsley[Boolean] = p.guardAgainst { case true => msg0 +: msgs }
}
/** This object makes "dot is zero" generate a bunch of given messages in a ''specialised'' error.
  * @since 4.1.0
  * @group doubledot
  */
object ZeroDotFail {
    def apply(msg0: String, msgs: String*): PreventDotIsZeroConfig = new ZeroDotFail(msg0, msgs: _*)
}

/** TODO:
  * @since 4.1.0
  * @group badchar
  */
sealed abstract class VerifiedBadChars {
    /** TODO:
      * @since 4.1.0
      * @group badchar
      */
    private [token] def checkBadChar: Parsley[Nothing]
}
private final class BadCharsFail private (cs: Map[Int, Seq[String]]) extends VerifiedBadChars {
    private [token] def checkBadChar: Parsley[Nothing] = satisfyUtf16(cs.contains).fail(cs.apply(_))
}
/** TODO:
  * @since 4.1.0
  * @group badchar
  */
object BadCharsFail {
    private [token] def apply(cs: Map[Int, Seq[String]]): VerifiedBadChars = if (cs.isEmpty) Unverified else new BadCharsFail(cs)
}

private final class BadCharsReason private (cs: Map[Int, String]) extends VerifiedBadChars {
    private [token] def checkBadChar: Parsley[Nothing] = satisfyUtf16(cs.contains)._unexpected(cs.apply)
}
/** TODO:
  * @since 4.1.0
  * @group badchar
  */
object BadCharsReason {
    def apply(cs: Map[Int, String]): VerifiedBadChars = if (cs.isEmpty) Unverified else new BadCharsReason(cs)
}

/** TODO:
  * @since 4.1.0
  * @group badchar
  */
object Unverified extends VerifiedBadChars {
    private [token] def checkBadChar: Parsley[Nothing] = empty
}

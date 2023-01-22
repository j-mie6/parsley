/* SPDX-FileCopyrightText: © 2023 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.errors

import parsley.Parsley, Parsley.empty
import parsley.character.satisfyUtf16
import parsley.errors.{combinator, patterns}, combinator.ErrorMethods, patterns.VerifiedErrors

/** This class is used to configure what error is generated when `.` is parsed as a real number.
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
    private [token] override def apply(p: Parsley[Boolean]): Parsley[Boolean] = p.unexpectedWhenWithReason {
        case true => (unexpected, reason)
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

/** This class is used to configure what error should be generated when illegal characters in a string or character literal are parsable.
  * @since 4.1.0
  * @group badchar
  */
sealed abstract class VerifiedBadChars {
    private [token] def checkBadChar: Parsley[Nothing]
}
private final class BadCharsFail private (cs: Map[Int, Seq[String]]) extends VerifiedBadChars {
    private [token] def checkBadChar: Parsley[Nothing] = satisfyUtf16(cs.contains).verifiedFail(cs.apply(_))
}
/** This object makes "bad literal chars" generate a bunch of given messages in a ''specialised'' error. Requires a map from bad characters to their messages.
  * @since 4.1.0
  * @group badchar
  */
object BadCharsFail {
    private [token] def apply(cs: Map[Int, Seq[String]]): VerifiedBadChars = if (cs.isEmpty) Unverified else new BadCharsFail(cs)
}

private final class BadCharsReason private (cs: Map[Int, String]) extends VerifiedBadChars {
    private [token] def checkBadChar: Parsley[Nothing] = satisfyUtf16(cs.contains).verifiedUnexpected(cs.apply(_))
}
/** This object makes "bad literal chars" generate a reason in a ''vanilla'' error. Requires a map from bad characters to their reasons.
  * @since 4.1.0
  * @group badchar
  */
object BadCharsReason {
    def apply(cs: Map[Int, String]): VerifiedBadChars = if (cs.isEmpty) Unverified else new BadCharsReason(cs)
}

/** This object disables the verified error for bad characters: this may improve parsing performance slightly on the failure case.
  * @since 4.1.0
  * @group badchar
  */
object Unverified extends VerifiedBadChars {
    private [token] def checkBadChar: Parsley[Nothing] = empty
}

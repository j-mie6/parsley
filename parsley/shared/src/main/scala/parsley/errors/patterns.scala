package parsley.errors

import parsley.Parsley

import parsley.internal.deepembedding.frontend

// TODO: document
object patterns {
    implicit final class VerifiedErrors[P, A](p: P)(implicit con: P => Parsley[A]) {
        private def verified(msggen: Either[A => Seq[String], Option[A => String]]) = new Parsley(new frontend.VerifiedError(con(p).internal, msggen))

        // partial amend semantics are BAD: they render the error in the wrong position unless amended anyway
        // Document that `attempt` may be used when this is an informative but not terminal error.
        def verifiedFail(msggen: A => Seq[String]): Parsley[Nothing] = verified(Left(msggen))
        def verifiedFail(msg: String, msgs: String*): Parsley[Nothing] = this.verifiedFail(_ => msg +: msgs)

        // TODO: Documentation and testing ahead of future release
        // like notFollowedBy, but does consume input on "success" and always fails
        // Document that `attempt` may be used when this is an informative but not terminal error.
        private def verifiedUnexpected(reason: Option[A => String]) = verified(Right(reason))
        def verifiedUnexpected: Parsley[Nothing] = this.verifiedUnexpected(None)
        def verifiedUnexpected(reason: String): Parsley[Nothing] = this.verifiedUnexpected(_ => reason)
        def verifiedUnexpected(reason: A => String): Parsley[Nothing] = this.verifiedUnexpected(Some(reason))
    }
}

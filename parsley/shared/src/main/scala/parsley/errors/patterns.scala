package parsley.errors

import parsley.Parsley

import parsley.internal.deepembedding.frontend

/** TODO:
  *
  * @since 4.2.0
  */
object patterns {
    /** TODO:
      *
      * @param p
      * @return
      * @since 4.2.0
      */
    implicit final class VerifiedErrors[P, A](p: P)(implicit con: P => Parsley[A]) {
        private def verified(msggen: Either[A => Seq[String], Option[A => String]]) = new Parsley(new frontend.VerifiedError(con(p).internal, msggen))
        private def verifiedUnexpected(reason: Option[A => String]) = verified(Right(reason))

        // partial amend semantics are BAD: they render the error in the wrong position unless amended anyway
        // Document that `attempt` may be used when this is an informative but not terminal error.
        /** TODO:
          *
          * @param msggen
          * @return
          * @since 4.2.0
          */
        def verifiedFail(msggen: A => Seq[String]): Parsley[Nothing] = verified(Left(msggen))

        /** TODO:
          *
          * @param msg0
          * @param msgs
          * @return
          * @since 4.2.0
          */
        def verifiedFail(msg0: String, msgs: String*): Parsley[Nothing] = this.verifiedFail(_ => msg0 +: msgs)

        // TODO: Documentation and testing ahead of future release
        // like notFollowedBy, but does consume input on "success" and always fails
        // Document that `attempt` may be used when this is an informative but not terminal error.

        /** TODO:
          *
          * @return
          * @since 4.2.0
          */
        def verifiedUnexpected: Parsley[Nothing] = this.verifiedUnexpected(None)

        /** TODO:
          *
          * @param reason
          * @return
          * @since 4.2.0
          */
        def verifiedUnexpected(reason: String): Parsley[Nothing] = this.verifiedUnexpected(_ => reason)

        /** TODO:
          *
          * @param reason
          * @return
          * @since 4.2.0
          */
        def verifiedUnexpected(reason: A => String): Parsley[Nothing] = this.verifiedUnexpected(Some(reason))
    }
}

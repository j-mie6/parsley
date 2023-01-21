package parsley.errors

import parsley.Parsley

import combinator.{partialAmend, ErrorMethods}

//import parsley.internal.deepembedding.{frontend, singletons}

// TODO: document
object patterns {
    implicit final class VerifiedErrors[P, +A](p: P)(implicit con: P => Parsley[A]) {
        // TODO: it should have the partial amend semantics, because `amendAndDislodge` can restore the other semantics anyway
        // Document that `attempt` may be used when this is an informative but not terminal error.
        private [parsley] def verifyFail(msggen: A => Seq[String]): Parsley[Nothing] = partialAmend {
            // holy hell, the hoops I jump through to be able to implement things
            val r = parsley.registers.Reg.make[(Int, A, Int)]
            val fails = Parsley.notFollowedBy(r.put(parsley.position.internalOffsetSpan(con(p).hide)))
            (fails <|> r.get.flatMap { case (os, x, oe) =>
                val msg0 +: msgs = msggen(x)
                combinator.fail(oe - os, msg0, msgs: _*)
            }) *> Parsley.empty
        }
        private [parsley] def verifyFail(msg: String, msgs: String*): Parsley[Nothing] = this.verifyFail(_ => msg +: msgs)


        // TODO: Documentation and testing ahead of future release
        // like notFollowedBy, but does consume input on "success" and always fails (FIXME: this needs intrinsic support to get right)
        // it should also have the partial amend semantics, because `amendAndDislodge` can restore the other semantics anyway
        // Document that `attempt` may be used when this is an informative but not terminal error.
        private def verifyUnexpected(reason: Option[A => String]) = partialAmend {
            // holy hell, the hoops I jump through to be able to implement things
            val r = parsley.registers.Reg.make[A]
            val fails = Parsley.notFollowedBy(r.put(con(p).hide))
            reason.fold(fails)(rgen => fails <|> r.get.flatMap(x => Parsley.empty.explain(rgen(x)))) *> Parsley.empty
        }
        private [parsley] def verifyUnexpected: Parsley[Nothing] = this.verifyUnexpected(None)
        private [parsley] def verifyUnexpected(reason: String): Parsley[Nothing] = this.verifyUnexpected(_ => reason)
        private [parsley] def verifyUnexpected(reason: A => String): Parsley[Nothing] = this.verifyUnexpected(Some(reason))
    }
}
package parsley.expr

import parsley.Parsley
import parsley.combinator.choice

object OriginalGOps {
    def apply[A, B](fixity: Fixity)(op0: Parsley[fixity.Op[A, B]], ops: Parsley[fixity.Op[A, B]]*)(implicit wrap: A => B): OriginalOps[A, B] = new OriginalOps[A, B] {
        private [expr] def chain(p: Parsley[A]): Parsley[B] = fixity.chain(p, choice((op0 +: ops): _*))
    }
}

object OriginalSOps {
    def apply[B, A <: B](fixity: Fixity)(op0: Parsley[fixity.Op[A, B]], ops: Parsley[fixity.Op[A, B]]*): OriginalOps[A, B] = OriginalGOps(fixity)(op0, ops: _*)
}

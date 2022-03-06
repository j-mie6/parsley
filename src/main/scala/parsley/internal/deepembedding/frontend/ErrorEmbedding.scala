package parsley.internal.deepembedding.frontend

import parsley.internal.deepembedding.ContOps.{result, ContAdapter}

import parsley.internal.deepembedding.backend, backend.StrictParsley

private [parsley] final class ErrorLabel[A](p: LazyParsley[A], private [ErrorLabel] val label: String) extends ScopedUnary[A, A](p) {
    override def name = s"label($label)"
    override def make(p: StrictParsley[A]) = new backend.ErrorLabel(p, label)
}
private [parsley] final class ErrorExplain[A](p: LazyParsley[A], reason: String) extends ScopedUnary[A, A](p) {
    override def name = s"explain($reason)"
    override def make(p: StrictParsley[A]) = new backend.ErrorExplain(p, reason)
}

private [parsley] final class ErrorAmend[A](p: LazyParsley[A]) extends ScopedUnary[A, A](p) {
    override val name = "amend"
    override def make(p: StrictParsley[A]) = new backend.ErrorAmend(p)
}
private [parsley] final class ErrorEntrench[A](p: LazyParsley[A]) extends ScopedUnary[A, A](p) {
    override val name = "entrench"
    override def make(p: StrictParsley[A]) = new backend.ErrorEntrench(p)
}
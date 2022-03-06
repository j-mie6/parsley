package parsley.internal.deepembedding.frontend

import parsley.internal.deepembedding.ContOps.{result, ContAdapter}

import parsley.internal.deepembedding.backend

private [parsley] final class ErrorLabel[A](p: LazyParsley[A], private [ErrorLabel] val label: String) extends ScopedUnary[A, A](p, new backend.ErrorLabel(_, label)) {
    override def name = s"label($label)"
}
private [parsley] final class ErrorExplain[A](p: LazyParsley[A], reason: String) extends ScopedUnary[A, A](p, new backend.ErrorExplain(_, reason)) {
    override def name = s"explain($reason)"
}

private [parsley] final class ErrorAmend[A](p: LazyParsley[A]) extends ScopedUnary[A, A](p, new backend.ErrorAmend(_)) {
    override val name = "amend"
}
private [parsley] final class ErrorEntrench[A](p: LazyParsley[A]) extends ScopedUnary[A, A](p, new backend.ErrorEntrench(_)) {
    override val name = "entrench"
}
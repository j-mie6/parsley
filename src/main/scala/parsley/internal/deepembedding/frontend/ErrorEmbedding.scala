package parsley.internal.deepembedding.frontend

import parsley.internal.deepembedding.ContOps.{result, ContAdapter}

import parsley.internal.deepembedding.backend

private [parsley] final class ErrorLabel[A](p: LazyParsley[A], private [ErrorLabel] val label: String)
    extends ScopedUnary[A, A](p, s"label($label)", new backend.ErrorLabel(_, label))
private [parsley] final class ErrorExplain[A](p: LazyParsley[A], reason: String)
    extends ScopedUnary[A, A](p, s"explain($reason)", new backend.ErrorExplain(_, reason))

private [parsley] final class ErrorAmend[A](p: LazyParsley[A]) extends ScopedUnary[A, A](p, "amend", new backend.ErrorAmend(_))
private [parsley] final class ErrorEntrench[A](p: LazyParsley[A]) extends ScopedUnary[A, A](p, "entrench", new backend.ErrorEntrench(_))
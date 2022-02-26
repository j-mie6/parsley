package parsley.internal.deepembedding

import ContOps.{result, ContAdapter}

private [parsley] final class Fail(private [Fail] val msgs: String*)
    extends Singleton[Nothing](s"fail(${msgs.mkString(", ")})", new backend.Fail(msgs: _*))

private [parsley] final class Unexpected(private [Unexpected] val msg: String)
    extends Singleton[Nothing](s"unexpected($msg)", new backend.Unexpected(msg))

private [parsley] final class ErrorLabel[A](p: Parsley[A], private [ErrorLabel] val label: String) extends ScopedUnary[A, A](p, s"label($label)", new backend.ErrorLabel(_, label))
private [parsley] final class ErrorExplain[A](p: Parsley[A], reason: String) extends ScopedUnary[A, A](p, s"explain($reason)", new backend.ErrorExplain(_, reason))

private [parsley] final class ErrorAmend[A](p: Parsley[A]) extends ScopedUnary[A, A](p, "amend", new backend.ErrorAmend(_))
private [parsley] final class ErrorEntrench[A](p: Parsley[A]) extends ScopedUnary[A, A](p, "entrench", new backend.ErrorEntrench(_))
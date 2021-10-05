package parsley.internal.deepembedding

import ContOps.{result, ContAdapter}
import parsley.internal.machine.instructions

private [parsley] final class Fail(private [Fail] val msgs: String*)
    extends Singleton[Nothing](s"fail(${msgs.mkString(", ")})", new instructions.Fail(msgs: _*)) with MZero

private [parsley] final class Unexpected(private [Unexpected] val msg: String)
    extends Singleton[Nothing](s"unexpected($msg)", new instructions.Unexpected(msg)) with MZero

private [parsley] final class ErrorLabel[A](private [deepembedding] var p: Parsley[A], private [ErrorLabel] val label: String)
    extends ScopedUnary[A, A](s"label($label)", new ErrorLabel(_, label), new instructions.InputCheck(_, true), new instructions.ApplyError(label)) {
    final override def optimise: Parsley[A] = p match {
        case ct@CharTok(c) if !ct.expected.contains("") => new CharTok(c, Some(label)).asInstanceOf[Parsley[A]]
        case st@StringTok(s) if !st.expected.contains("") => new StringTok(s, Some(label)).asInstanceOf[Parsley[A]]
        case sat@Satisfy(f) if !sat.expected.contains("") => new Satisfy(f, Some(label)).asInstanceOf[Parsley[A]]
        case ErrorLabel(p, label2) if label2 != "" => ErrorLabel(p, label)
        case _ => this
    }
}
private [parsley] final class ErrorExplain[A](private [deepembedding] var p: Parsley[A], reason: String)
    extends ScopedUnary[A, A](s"explain($reason)", new ErrorExplain(_, reason), new instructions.InputCheck(_), new instructions.ApplyReason(reason))

private [parsley] final class ErrorAmend[A](private [deepembedding] var p: Parsley[A]) extends ScopedUnaryWithState[A, A]("amend", false, new ErrorAmend(_), instructions.Amend)
private [parsley] final class ErrorEntrench[A](private [deepembedding] var p: Parsley[A])
    extends ScopedUnary[A, A]("entrench", new ErrorEntrench(_), new instructions.PushHandler(_), instructions.Entrench)

private [deepembedding] object ErrorLabel {
    def apply[A](p: Parsley[A], label: String): ErrorLabel[A] = new ErrorLabel(p, label).ready()
    def unapply[A](self: ErrorLabel[A]): Some[(Parsley[A], String)] = Some((self.p, self.label))
}
private [deepembedding] object ErrorExplain {
    def apply[A](p: Parsley[A], reason: String): ErrorExplain[A] = new ErrorExplain(p, reason).ready()
}
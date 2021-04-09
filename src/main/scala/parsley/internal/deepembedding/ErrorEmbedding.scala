package parsley.internal.deepembedding

import ContOps.{result, ContAdapter}
import parsley.internal.machine.instructions

private [parsley] final class Fail(private [Fail] val msgs: String*)
    extends Singleton[Nothing](s"fail(${msgs.mkString(", ")})", new instructions.Fail(msgs: _*)) with MZero

private [parsley] final class Unexpected(private [Unexpected] val msg: String)
    extends Singleton[Nothing](s"unexpected($msg)", new instructions.Unexpected(msg)) with MZero

private [parsley] final class ErrorLabel[A](_p: =>Parsley[A], private [ErrorLabel] val label: String)
    extends ScopedUnary[A, A](_p, s"label($label)", ErrorLabel.empty(label), new instructions.InputCheck(_, true), new instructions.ApplyError(label)) {
    final override def optimise: Parsley[A] = p match {
        case ct@CharTok(c) if !ct.expected.contains("") => new CharTok(c, Some(label)).asInstanceOf[Parsley[A]]
        case st@StringTok(s) if !st.expected.contains("") => new StringTok(s, Some(label)).asInstanceOf[Parsley[A]]
        case sat@Satisfy(f) if !sat.expected.contains("") => new Satisfy(f, Some(label)).asInstanceOf[Parsley[A]]
        case ErrorLabel(p, label2) if label2 != "" => ErrorLabel(p, label)
        case _ => this
    }
}
private [parsley] final class ErrorExplain[A](_p: =>Parsley[A], reason: String)
    extends ScopedUnary[A, A](_p, s"explain($reason)", ErrorExplain.empty(reason), new instructions.InputCheck(_), new instructions.ApplyReason(reason))

private [parsley] final class ErrorAmend[A](_p: =>Parsley[A]) extends ScopedUnaryWithState[A, A](_p, "amend", false, ErrorAmend.empty, instructions.Amend)
private [parsley] final class ErrorEntrench[A](_p: =>Parsley[A])
    extends ScopedUnary[A, A](_p, "entrench", ErrorEntrench.empty, new instructions.PushHandler(_), instructions.Entrench)

private [deepembedding] object ErrorLabel {
    def empty[A](label: String): ErrorLabel[A] = new ErrorLabel(???, label)
    def apply[A](p: Parsley[A], label: String): ErrorLabel[A] = empty(label).ready(p)
    def unapply[A](self: ErrorLabel[A]): Some[(Parsley[A], String)] = Some((self.p, self.label))
}
private [deepembedding] object ErrorExplain {
    def empty[A](reason: String): ErrorExplain[A] = new ErrorExplain(???, reason)
    def apply[A](p: Parsley[A], reason: String): ErrorExplain[A] = empty(reason).ready(p)
}
private [deepembedding] object ErrorAmend {
    def empty[A]: ErrorAmend[A] = new ErrorAmend(???)
}
private [deepembedding] object ErrorEntrench {
    def empty[A]: ErrorEntrench[A] = new ErrorEntrench(???)
}
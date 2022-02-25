package parsley.internal.deepembedding.backend

import parsley.internal.deepembedding.ContOps.{result, ContAdapter}
import parsley.internal.machine.instructions

private [parsley] final class Fail(private [Fail] val msgs: String*)
    extends Singleton[Nothing](new instructions.Fail(msgs: _*)) with MZero

private [parsley] final class Unexpected(private [Unexpected] val msg: String)
    extends Singleton[Nothing](new instructions.Unexpected(msg)) with MZero

private [parsley] final class ErrorLabel[A](var p: StrictParsley[A], private [ErrorLabel] val label: String)
    extends ScopedUnary[A, A](new instructions.InputCheck(_, true), new instructions.ApplyError(label)) {
    final override def optimise: StrictParsley[A] = p match {
        case ct@CharTok(c) if !ct.expected.contains("") => new CharTok(c, Some(label)).asInstanceOf[StrictParsley[A]]
        case st@StringTok(s) if !st.expected.contains("") => new StringTok(s, Some(label)).asInstanceOf[StrictParsley[A]]
        case sat@Satisfy(f) if !sat.expected.contains("") => new Satisfy(f, Some(label)).asInstanceOf[StrictParsley[A]]
        // TOOD: The hide property is required to be checked, but there is no test for it
        case ErrorLabel(p, label2) if label2 != "" => ErrorLabel(p, label)
        case _ => this
    }
}
private [parsley] final class ErrorExplain[A](var p: StrictParsley[A], reason: String)
    extends ScopedUnary[A, A](new instructions.InputCheck(_), new instructions.ApplyReason(reason))

private [parsley] final class ErrorAmend[A](var p: StrictParsley[A]) extends ScopedUnaryWithState[A, A](false, instructions.Amend)
private [parsley] final class ErrorEntrench[A](var p: StrictParsley[A])
    extends ScopedUnary[A, A](new instructions.PushHandler(_), instructions.Entrench)

private [deepembedding] object ErrorLabel {
    def apply[A](p: StrictParsley[A], label: String): ErrorLabel[A] = new ErrorLabel(p, label)
    def unapply[A](self: ErrorLabel[A]): Some[(StrictParsley[A], String)] = Some((self.p, self.label))
}
private [deepembedding] object ErrorExplain {
    def apply[A](p: StrictParsley[A], reason: String): ErrorExplain[A] = new ErrorExplain(p, reason)
}
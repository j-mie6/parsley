package parsley.internal.deepembedding.backend

import parsley.internal.deepembedding.singletons._
import parsley.internal.machine.instructions
private [deepembedding] final class ErrorLabel[A](val p: StrictParsley[A], private [ErrorLabel] val label: String) extends ScopedUnary[A, A] {
    override def setup(label: Int) = new instructions.InputCheck(label, true)
    override def instr = new instructions.ApplyError(label)
    final override def optimise: StrictParsley[A] = p match {
        case ct@CharTok(c) if !ct.expected.contains("") => new CharTok(c, Some(label)).asInstanceOf[StrictParsley[A]]
        case st@StringTok(s) if !st.expected.contains("") => new StringTok(s, Some(label)).asInstanceOf[StrictParsley[A]]
        case sat@Satisfy(f) if !sat.expected.contains("") => new Satisfy(f, Some(label)).asInstanceOf[StrictParsley[A]]
        // TOOD: The hide property is required to be checked, but there is no test for it
        case ErrorLabel(p, label2) if label2 != "" => ErrorLabel(p, label)
        case _ => this
    }
}
private [deepembedding] final class ErrorExplain[A](val p: StrictParsley[A], reason: String) extends ScopedUnary[A, A] {
    override def setup(label: Int) = new instructions.InputCheck(label)
    override def instr = new instructions.ApplyReason(reason)
}

private [deepembedding] final class ErrorAmend[A](val p: StrictParsley[A]) extends ScopedUnaryWithState[A, A](false) {
    override val instr = instructions.Amend
}
private [deepembedding] final class ErrorEntrench[A](val p: StrictParsley[A]) extends ScopedUnary[A, A] {
    override def setup(label: Int) = new instructions.PushHandler(label)
    override val instr = instructions.Entrench
}

private [backend] object ErrorLabel {
    def apply[A](p: StrictParsley[A], label: String): ErrorLabel[A] = new ErrorLabel(p, label)
    def unapply[A](self: ErrorLabel[A]): Some[(StrictParsley[A], String)] = Some((self.p, self.label))
}
private [backend] object ErrorExplain {
    def apply[A](p: StrictParsley[A], reason: String): ErrorExplain[A] = new ErrorExplain(p, reason)
}
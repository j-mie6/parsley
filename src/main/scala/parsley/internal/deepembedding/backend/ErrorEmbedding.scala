/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.backend

import parsley.internal.deepembedding.singletons._
import parsley.internal.machine.instructions
private [deepembedding] final class ErrorLabel[A](val p: StrictParsley[A], private [ErrorLabel] val label: String) extends ScopedUnary[A, A] {
    override def setup(label: Int): instructions.Instr = new instructions.PushHandlerAndCheck(label, saveHints = true)
    override def instr: instructions.Instr = new instructions.RelabelHints(label)
    override def instrNeedsLabel: Boolean = false
    override def handlerLabel(state: CodeGenState): Int = state.getLabelForRelabelError(label)
    final override def optimise: StrictParsley[A] = p match {
        case ct@CharTok(c) if !ct.expected.contains("") => new CharTok(c, Some(label)).asInstanceOf[StrictParsley[A]]
        case st@StringTok(s) if !st.expected.contains("") => new StringTok(s, Some(label)).asInstanceOf[StrictParsley[A]]
        case sat@Satisfy(f) if !sat.expected.contains("") => new Satisfy(f, Some(label)).asInstanceOf[StrictParsley[A]]
        // TOOD: The hide property is required to be checked, but there is no test for it
        case ErrorLabel(p, label2) if label2 != "" => ErrorLabel(p, label)
        case _ => this
    }

    // $COVERAGE-OFF$
    final override def pretty(p: String): String = s"$p.label($label)"
    // $COVERAGE-ON$
}
private [deepembedding] final class ErrorExplain[A](val p: StrictParsley[A], reason: String) extends ScopedUnary[A, A] {
    override def setup(label: Int): instructions.Instr = new instructions.PushHandlerAndCheck(label, saveHints = false)
    override def instr: instructions.Instr = instructions.PopHandlerAndCheck
    override def instrNeedsLabel: Boolean = false
    override def handlerLabel(state: CodeGenState): Int  = state.getLabelForApplyReason(reason)
    // $COVERAGE-OFF$
    final override def pretty(p: String): String = s"$p.explain($reason)"
    // $COVERAGE-ON$
}

private [deepembedding] final class ErrorAmend[A](val p: StrictParsley[A]) extends ScopedUnaryWithState[A, A](false) {
    override val instr: instructions.Instr = instructions.PopHandlerAndState
    override def instrNeedsLabel: Boolean = false
    override def handlerLabel(state: CodeGenState): Int  = state.getLabel(instructions.AmendAndFail)
    // $COVERAGE-OFF$
    final override def pretty(p: String): String = s"amend($p)"
    // $COVERAGE-ON$
}
private [deepembedding] final class ErrorEntrench[A](val p: StrictParsley[A]) extends ScopedUnary[A, A] {
    override def setup(label: Int): instructions.Instr = new instructions.PushHandler(label)
    override val instr: instructions.Instr = instructions.PopHandler
    override def instrNeedsLabel: Boolean = false
    override def handlerLabel(state: CodeGenState): Int  = state.getLabel(instructions.EntrenchAndFail)
    // $COVERAGE-OFF$
    final override def pretty(p: String): String = s"entrench($p)"
    // $COVERAGE-ON$
}

private [backend] object ErrorLabel {
    def apply[A](p: StrictParsley[A], label: String): ErrorLabel[A] = new ErrorLabel(p, label)
    def unapply[A](self: ErrorLabel[A]): Some[(StrictParsley[A], String)] = Some((self.p, self.label))
}
private [backend] object ErrorExplain {
    def apply[A](p: StrictParsley[A], reason: String): ErrorExplain[A] = new ErrorExplain(p, reason)
}

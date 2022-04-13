package parsley.internal.deepembedding.singletons

import parsley.internal.deepembedding.backend.MZero
import parsley.internal.machine.instructions

private [parsley] final class Fail(private [Fail] val msgs: String*) extends Singleton[Nothing] with MZero {
    // $COVERAGE-OFF$
    override def pretty: String = s"fail(${msgs.mkString(", ")})"
    // $COVERAGE-ON$
    override def instr: instructions.Instr = new instructions.Fail(msgs: _*)
}

private [parsley] final class Unexpected(private [Unexpected] val msg: String) extends Singleton[Nothing] with MZero {
    // $COVERAGE-OFF$
    override def pretty: String = s"unexpected($msg)"
    // $COVERAGE-ON$
    override def instr: instructions.Instr = new instructions.Unexpected(msg)
}

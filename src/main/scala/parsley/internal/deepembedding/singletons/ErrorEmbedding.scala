package parsley.internal.deepembedding.singletons

import parsley.internal.machine.instructions
import parsley.internal.deepembedding.backend.MZero

private [parsley] final class Fail(private [Fail] val msgs: String*) extends Singleton[Nothing] with MZero {
    // $COVERAGE-OFF$
    override def pretty = s"fail(${msgs.mkString(", ")})"
    // $COVERAGE-ON$
    override def instr = new instructions.Fail(msgs: _*)
}

private [parsley] final class Unexpected(private [Unexpected] val msg: String) extends Singleton[Nothing] with MZero {
    // $COVERAGE-OFF$
    override def pretty = s"unexpected($msg)"
    // $COVERAGE-ON$
    override def instr = new instructions.Unexpected(msg)
}
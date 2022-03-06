package parsley.internal.deepembedding.singletons

import parsley.internal.machine.instructions
import parsley.internal.deepembedding.backend.MZero

private [parsley] final class Fail(private [Fail] val msgs: String*)
    extends Singleton[Nothing](s"fail(${msgs.mkString(", ")})", new instructions.Fail(msgs: _*)) with MZero

private [parsley] final class Unexpected(private [Unexpected] val msg: String)
    extends Singleton[Nothing](s"unexpected($msg)", new instructions.Unexpected(msg)) with MZero
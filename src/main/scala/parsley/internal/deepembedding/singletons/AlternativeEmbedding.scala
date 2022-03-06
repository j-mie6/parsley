package parsley.internal.deepembedding.singletons

import parsley.internal.machine.instructions
import parsley.internal.deepembedding.backend.MZero

private [parsley] object Empty extends Singleton[Nothing] with MZero {
    // $COVERAGE-OFF$
    override val pretty = "empty"
    // $COVERAGE-ON$
    override val instr = instructions.Empty
}
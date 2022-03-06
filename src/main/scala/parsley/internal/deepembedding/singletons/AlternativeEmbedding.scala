package parsley.internal.deepembedding.singletons

import parsley.internal.machine.instructions
import parsley.internal.deepembedding.backend.MZero

private [parsley] object Empty extends Singleton[Nothing]("empty", instructions.Empty) with MZero
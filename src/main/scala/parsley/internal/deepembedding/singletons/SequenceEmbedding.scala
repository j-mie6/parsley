package parsley.internal.deepembedding.singletons

import parsley.internal.deepembedding.frontend
import parsley.internal.machine.instructions

// Core Embedding
private [parsley] final class Pure[A](private [Pure] val x: A) extends Singleton[A] {
    override def pretty = s"pure($x)"
    override def instr = new instructions.Push(x)
}

private [deepembedding] object Pure {
    def unapply[A](self: Pure[A]): Some[A] = Some(self.x)
}
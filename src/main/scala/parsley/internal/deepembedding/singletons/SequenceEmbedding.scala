package parsley.internal.deepembedding.singletons

import parsley.internal.deepembedding.frontend
import parsley.internal.machine.instructions

// Core Embedding
private [parsley] final class Pure[A](private [Pure] val x: A) extends Singleton[A] {
    // $COVERAGE-OFF$
    override def pretty: String = s"pure($x)"
    // $COVERAGE-ON$
    override def instr: instructions.Instr = new instructions.Push(x)
}

private [deepembedding] object Pure {
    def unapply[A](self: Pure[A]): Some[A] = Some(self.x)
}

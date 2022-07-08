/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.singletons

import parsley.internal.machine.instructions

// Core Embedding
private [parsley] final class Pure[A](private [Pure] val x: A) extends Singleton[A] {
    // $COVERAGE-OFF$
    override def pretty: String = s"pure($x)"
    // $COVERAGE-ON$
    override def instr: instructions.Instr = new instructions.Push(x)
}

private [parsley] final class Fresh[A](x: =>A) extends Singleton[A] {
    // $COVERAGE-OFF$
    override def pretty: String = s"fresh($x)"
    // $COVERAGE-ON$
    override def instr: instructions.Instr = new instructions.Fresh(x)
}

private [deepembedding] object Pure {
    def unapply[A](self: Pure[A]): Some[A] = Some(self.x)
}

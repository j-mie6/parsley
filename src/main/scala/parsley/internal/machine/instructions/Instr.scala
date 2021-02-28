package parsley.internal.machine.instructions

import parsley.internal.machine.Context

private [internal] abstract class Instr {
    def apply(ctx: Context): Unit
    def relabel(labels: Array[Int]): this.type = this
    // Instructions should override this if they have mutable state inside!
    def copy: Instr = this
}

private [internal] trait Stateful

private [internal] abstract class InstrWithLabel extends Instr {
    var label: Int
    override def relabel(labels: Array[Int]): this.type = {
        label = labels(label)
        this
    }
}

// It's 2018 and Labels are making a come-back, along with 2 pass assembly
private [internal] final class Label(val i: Int) extends Instr {
    // $COVERAGE-OFF$
    def apply(ctx: Context): Unit = throw new Exception("Cannot execute label")
    // $COVERAGE-ON$
}
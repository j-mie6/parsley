package parsley.internal

import scala.language.implicitConversions

package object instructions
{
    private [instructions] sealed abstract class Status
    private [instructions] case object Good extends Status
    private [instructions] case object Recover extends Status
    private [instructions] case object Failed extends Status

    private [internal] abstract class Instr {
        def apply(ctx: Context): Unit
        def relabel(labels: Array[Int]): Unit = ()
        // Instructions should override this if they have mutable state inside!
        def copy: Instr = this
    }

    private [internal] trait Stateful

    private [internal] abstract class JumpInstr extends Instr {
        var label: Int
        override def relabel(labels: Array[Int]): Unit = label = labels(label)
    }

    // It's 2018 and Labels are making a come-back, along with 2 pass assembly
    private [internal] final class Label(val i: Int) extends Instr {
        // $COVERAGE-OFF$
        def apply(ctx: Context): Unit = throw new Exception("Cannot execute label")
        // $COVERAGE-ON$
    }

    // $COVERAGE-OFF$
    final private [parsley] def pretty(instrs: Array[Instr]): String = {
        val n = instrs.length
        val digits = if (n != 0) Math.log10(n).toInt + 1 else 0
        instrs.zipWithIndex.map {
            case (instr, idx) =>
                val paddedIdx = {
                    val str = idx.toString
                    " " * (digits - str.length) + str
                }
                val paddedHex = {
                    val str = instr.##.toHexString
                    "0" * (8 - str.length) + str
                }
                s"$paddedIdx [$paddedHex]: $instr"
        }.mkString(";\n")
    }
    // $COVERAGE-ON$

    final private [internal] def stateSafeCopy(instrs: Array[Instr], pindices: Array[Int]): Array[Instr] = {
        val nstateful = pindices.length
        if (nstateful != 0) {
            val instrs_ = instrs.clone
            for (i <- 0 until nstateful) {
                val j = pindices(i)
                instrs_(j) = instrs(j).copy
            }
            instrs_
        }
        else instrs
    }

    final private [internal] def statefulIndices(instrs: Array[Instr]): Array[Int] = {
        val buff = new ResizableArray[Int]()
        for (i <- 0 until instrs.length) {
            if (instrs(i).isInstanceOf[Stateful]) buff += i
        }
        buff.toArray
    }
}

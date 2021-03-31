package parsley.internal.machine

import parsley.internal.ResizableArray

import scala.language.implicitConversions

package object instructions
{
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
        buff.toShrunkenArray
    }
}

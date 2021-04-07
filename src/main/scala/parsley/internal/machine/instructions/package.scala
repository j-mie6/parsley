package parsley.internal.machine

import parsley.internal.ResizableArray

import scala.language.implicitConversions
import scala.util.control.Breaks.{breakable, break}
import scala.collection.mutable

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

    final private [internal] def statefulIndicesToReturn(instrs: Array[Instr], start: Int): List[Int] = {
        val buff = mutable.ListBuffer.empty[Int]
        breakable {
            for (i <- start until instrs.length) instrs(i) match {
                case _: Stateful => buff += i
                case Return => break()
                case _ =>
            }
        }
        buff.toList
    }

    final private [internal] def dependencies(instrs: Array[Instr], start: Int): Set[Int] = {
        val deps = mutable.Set.empty[Int]
        breakable {
            for (i <- start until instrs.length) instrs(i) match {
                case sub: GoSub => deps += sub.label
                case Return => break()
                case _ =>
            }
        }
        deps.toSet
    }
}

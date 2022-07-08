/* SPDX-FileCopyrightText: © 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine

package object instructions {
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
}

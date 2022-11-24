/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.errors

// TODO: I'd like to put these in modules to make them a little nicer

sealed abstract class TokenSpan {
    private [parsley] def toCaretLength(line: Int, col: Int, lengthLine: Int, lengthAfters: =>List[Int]): Int
}
object TokenSpan {
    case class Width(w: Int) extends TokenSpan {
        override private [parsley] def toCaretLength(line: Int, col: Int, lengthLine: Int, lengthAfters: =>List[Int]): Int = w
    }
    // Make clear that this isn't until pos, it's more like "after n lines and m cols" (we start reparsing at (1, 1))
    case class UntilPos(line: Int, col: Int) extends TokenSpan {
        private def correctedLine = line - 1
        private def correctedCol = col - 1
        override private [parsley] def toCaretLength(line: Int, col: Int, lengthLine: Int, lengthAfters: =>List[Int]): Int = {
            if (correctedLine == 0) correctedCol
            else {
                val adjustedLineLength = lengthLine - (col - 1)
                val _lengthAfters = lengthAfters
                val firstSize = adjustedLineLength - correctedCol
                if (correctedLine > _lengthAfters.length) {
                    firstSize + _lengthAfters.sum
                }
                else {
                    val intermediateSize = _lengthAfters.take(correctedLine - 1).sum
                    val lastSize = correctedCol
                    firstSize + intermediateSize + lastSize
                }
            }
        }
    }
}

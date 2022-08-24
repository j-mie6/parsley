/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.errors

sealed abstract class TokenSpan {
    private [parsley] def toCaretLength(line: Int, col: Int, lengthLine: Int, lengthAfters: =>List[Int]): Int
}
case class Width(w: Int) extends TokenSpan {
    override private [parsley] def toCaretLength(line: Int, col: Int, lengthLine: Int, lengthAfters: =>List[Int]): Int = w
}
case class UntilPos(line: Int, col: Int) extends TokenSpan {
    override private [parsley] def toCaretLength(line: Int, col: Int, lengthLine: Int, lengthAfters: =>List[Int]): Int = {
        val nlines = this.line - line
        if (nlines == 0) this.col - col + 1
        else {
            val _lengthAfters = lengthAfters
            val firstSize = lengthLine - col
            if (nlines > _lengthAfters.length) {
                firstSize + _lengthAfters.sum
            }
            else {
                val intermediateSize = _lengthAfters.take(nlines-1).sum
                val lastSize = this.col
                firstSize + intermediateSize + lastSize
            }
        }
    }
}

sealed abstract class Token {
    def span: TokenSpan
}
case class Raw(tok: String) extends Token {
    override def span: TokenSpan = {
        val idx = tok.indexOf('\n')
        Width(if (idx != -1) idx+1 else tok.length)
    }
}
case class Named(name: String, span: TokenSpan) extends Token
case object EndOfInput extends Token {
    override def span: TokenSpan = Width(1)
}

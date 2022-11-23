/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.errors

// TODO: I'd like to put these in modules to make them a little nicer

sealed abstract class Token {
    def span: TokenSpan
}
case class Raw(tok: String) extends Token {
    override def span: TokenSpan = {
        val idx = tok.indexOf('\n')
        Width(tok.codePointCount(0, if (idx != -1) idx + 1 else tok.length))
    }
}
case class Named(name: String, span: TokenSpan) extends Token
case object EndOfInput extends Token {
    override def span: TokenSpan = Width(1)
}

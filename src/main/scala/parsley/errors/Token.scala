/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.errors

sealed abstract class Token {
    def width: Int
}
case class Raw(tok: String) extends Token {
    override def width: Int = tok.length
}
case class Named(name: String, width: Int) extends Token
case object EndOfInput extends Token {
    override def width: Int = 1 // could be 0?
}

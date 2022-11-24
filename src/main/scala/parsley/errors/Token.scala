/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.errors

/** TODO:
  *
  * @since 4.0.0
  */
sealed abstract class Token {
    private [parsley] def span: TokenSpan
}
/** TODO:
  *
  * @since 4.0.0
  */
object Token {
    /** TODO:
      *
      * @param tok
      * @since 4.0.0
      */
    final case class Raw(tok: String) extends Token {
        override private [parsley] def span: TokenSpan = {
            val idx = tok.indexOf('\n')
            TokenSpan.Width(tok.codePointCount(0, if (idx != -1) idx + 1 else tok.length))
        }
    }
    /** TODO:
      *
      * @param name
      * @param span
      * @since 4.0.0
      */
    final case class Named(name: String, span: TokenSpan) extends Token
    /** TODO:
      *
      * @since 4.0.0
      */
    case object EndOfInput extends Token {
        override private [parsley] def span: TokenSpan = TokenSpan.Width(1)
    }
}

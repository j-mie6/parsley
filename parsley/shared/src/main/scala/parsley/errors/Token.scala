/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.errors

// TODO: move into ErrorBuilder object in 5.5.0?
/** This class represents an extracted token returned by `unexpectedToken` in `ErrorBuilder`.
  *
  * There is deliberately no analogue for `EndOfInput` because we guarantee that non-empty
  * residual input is provided to token extraction.
  *
  * @since 4.0.0
  * @group token
  */
sealed abstract class Token {
    private [parsley] def span: TokenSpan
}
/** This object contains the sub-types of `Token`.
  * @since 4.0.0
  * @group token
  */
object Token {
    /** This is a token that is directly extracted from the residual input itself.
      *
      * @param tok the input extracted
      * @since 4.0.0
      */
    final case class Raw(tok: String) extends Token {
        require(tok.nonEmpty, "raw tokens must always be non-empty")
        override private [parsley] def span: TokenSpan = {
            // TODO: line trimming isn't needed if carets are handled properly with respect to the codepoints
            val idx = tok.indexOf('\n')
            TokenSpan.Width(tok.codePointCount(0, if (idx != -1) idx + 1 else tok.length))
        }
    }
    /** This is a token that has been given a name, and is treated like a labelled
      * item.
      *
      * @param name the description of the token
      * @param span the amount of residual input this token ate
      * @since 4.0.0
      */
    final case class Named(name: String, span: TokenSpan) extends Token
}

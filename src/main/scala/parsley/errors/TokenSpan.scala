/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.errors

/** This class and its subtypes are used to describe how big a token should be (and therefore the
  * size of the corresponding error caret). This is provided to `Token.Named` and is therefore
  * used in the creation of `Token`s for lexical extractors.
  *
  * @since 4.0.0
  * @group token
  */
sealed abstract class TokenSpan {
    /** This method should compute the true length of a caret given a span and information about the error lines.
      *
      * @param col the column that the original error was raised at
      * @param lengthLine length of the line the error occured on
      * @param lengthAfters the length of the lines that follow
      * @return length of the caret
      */
    private [parsley] def toCaretLength(col: Int, lengthLine: Int, lengthAfters: =>List[Int]): Int
}
/** This object contains the sub-types of `TokenSpan`.
  * @since 4.0.0
  * @group token
  */
object TokenSpan {
    /** This class describes a simple span of `w` characters.
      *
      * @param w the width of the token in UTF-16 characters.
      * @since 4.0.0
      */
    final case class Width(w: Int) extends TokenSpan {
        override private [parsley] def toCaretLength(col: Int, lengthLine: Int, lengthAfters: =>List[Int]): Int = w
    }

    /** This span is designed to be used by token extractors that try and parse the
      * remaining input: it indicates the number of lines and columns that were
      * parsed in the process of extracting the token.
      *
      * @param line the number of lines the token passes over
      * @param col the number of columns consumed on the most recent line
      * @note Parsley starts at position (1, 1): if the final position after token extraction on residual input was (2, 1)
      *       then that token would span 1 line and 0 columns.
      * @since 4.0.0
      */
    final case class Spanning(line: Int, col: Int) extends TokenSpan {
        require(line >= 0, "a token cannot have a negative line length")
        require(col >= 0, "a token cannot have a negative column length")
        override private [parsley] def toCaretLength(col: Int, lengthLine: Int, lengthAfters: =>List[Int]): Int = {
            if (this.line == 0) this.col
            else {
                val adjustedLineLength = lengthLine - (col - 1)
                val _lengthAfters = lengthAfters
                val firstSize = adjustedLineLength - this.col
                if (this.line > _lengthAfters.length) {
                    firstSize + _lengthAfters.sum
                }
                else {
                    val intermediateSize = _lengthAfters.take(this.line - 1).sum
                    val lastSize = this.col
                    firstSize + intermediateSize + lastSize
                }
            }
        }
    }
}

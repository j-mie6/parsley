/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debugger

import ParseAttempt._

/** A representation of the attempts a parser has made during parse-time.
  *
  * @param fromOffset This offset is where the parse attempt started in the input.
  * @param toOffset   This offset is where the parse attempt finished in the input.
  * @param fromPos    [[fromOffset]] represented as a (line, column) pair.
  * @param toPos      [[toOffset]] represented as a (line, column pair).
  * @param success    Was this parse attempt successful?
  * @param result     If this parse attempt was successful, what did it return?
  *                   It is guaranteed that `result.isDefined` is true if and only if the attempt
  *                   is successful.
  *
  * @since 4.5.0
  */
sealed trait ParseAttempt {
    /** The input parsed, as raw text. */
    val rawInput: Input

    /** This offset is where the parse attempt started in the input. */
    val fromOffset: Offset

    /** This offset is where the parse attempt finished in the input. */
    val toOffset: Offset

    /** [[fromOffset]] represented as a (line, column) pair. */
    val fromPos: Pos

    /** [[toOffset]] represented as a (line, column pair). */
    val toPos: Pos

    /** Was this parse attempt successful? */
    val success: Success

    /** If this parse attempt was successful, what did it return? It is guaranteed that `result.isDefined` is true
      * if and only if the attempt is successful.
      */
    val result: Result

    assert(success == result.isDefined)
}

object ParseAttempt {
    type Input   = String
    type Offset  = Int
    type Line    = Int
    type Column  = Int
    type Pos     = (Line, Column)
    type Success = Boolean
    type Result  = Option[Any]

    def unApply(att: ParseAttempt): Option[(Input, Offset, Offset, Pos, Pos, Success, Result)] =
        Some((att.rawInput, att.fromOffset, att.toOffset, att.fromPos, att.toPos, att.success, att.result))
}

// Internal representation a parser's attempt to parse some input.
private [parsley] class ParseAttemptImpl(
    override val rawInput: String,
    override val fromOffset: Int,
    override val toOffset: Int,
    override val fromPos: (Int, Int),
    override val toPos: (Int, Int),
    override val success: Boolean,
    // It is guaranteed by the debugger that success <=> result.isDefined.
    override val result: Option[Any]
) extends ParseAttempt {
    // Make sure this class has not been used improperly.
    assert(success == result.isDefined)
}

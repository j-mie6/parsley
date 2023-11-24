/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debugger

import ParseAttempt._ // scalastyle:ignore underscore.import

/** A representation of the attempts a parser has made during parse-time.
  *
  * @since 4.5.0
  */
class ParseAttempt private [parsley] (
    inp: Input,
    fof: Offset,
    tof: Offset,
    fps: Pos,
    tps: Pos,
    scs: Success,
    res: Result
) {
    /** The input parsed, as raw text. */
    val rawInput: Input = inp

    /** This offset is where the parse attempt started in the input. */
    val fromOffset: Offset = fof

    /** This offset is where the parse attempt finished in the input. */
    val toOffset: Offset = tof

    /** [[fromOffset]] represented as a (line, column) pair. */
    val fromPos: Pos = fps

    /** [[toOffset]] represented as a (line, column pair). */
    val toPos: Pos = tps

    /** Was this parse attempt successful?
      *
      * @note [[success]] if and only if [[result]] is defined (contains a value).
      */
    val success: Success = scs

    /** If this parse attempt was successful, what did it return? It is guaranteed that `result.isDefined` is true
      * if and only if the attempt is successful.
      *
      * @note [[success]] if and only if [[result]] is defined (contains a value).
      */
    val result: Result = res

    // Make sure this class has not been used improperly.
    assert(success == result.isDefined)

    // Utility copy method only to be used internally.
    private [parsley] def copy(
        inp: Input = rawInput,
        fof: Offset = fromOffset,
        tof: Offset = toOffset,
        fps: Pos = fromPos,
        tps: Pos = toPos,
        scs: Success = success,
        res: Result = result
    ): ParseAttempt =
        new ParseAttempt(inp, fof, tof, fps, tps, scs, res)
}

object ParseAttempt {
    type Input   = String
    type Offset  = Int
    type Line    = Int
    type Column  = Int
    type Pos     = (Line, Column)
    type Success = Boolean
    type Result  = Option[Any]

    // This gives you everything you need for inspecting a parse attempt made by a parser.
    def unapply(att: ParseAttempt): Option[(Input, Offset, Offset, Pos, Pos, Success, Result)] =
        Some((att.rawInput, att.fromOffset, att.toOffset, att.fromPos, att.toPos, att.success, att.result))
}

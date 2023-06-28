/* SPDX-FileCopyrightText: © 2023 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debugger

/** A representation of the attempts a parser has made during parse-time.
  *
  * @param rawInput   The input parsed, as raw text.
  * @param fromOffset This offset is where the parse attempt started in the input.
  * @param toOffset   This offset is where the parse attempt finished in the input.
  * @param fromPos    [[fromOffset]] represented as a (line, column) pair.
  * @param toPos      [[toOffset]] represented as a (line, column pair).
  * @param success    Was this parse attempt successful?
  * @param result     If this parse attempt was successful, what did it return?
  *                   It is guaranteed that `result.isDefined` is true if and only if the attempt
  *                   is successful.
  */
case class ParseAttempt
  ( rawInput: String
  , fromOffset: Int
  , toOffset: Int
  , fromPos: (Int, Int)
  , toPos: (Int, Int)
  , success: Boolean
  // It is guaranteed by the debugger that success <=> result.isDefined.
  , result: Option[Any]
  ) extends AnyRef {
  // Make sure this class has not been used improperly.
  assert(success == result.isDefined)

  // We want most of the benefits of a case class, but equals and hashCode are not what we want.
  // This allows disambiguation of attempts in a list, as they are all supposed to be "unique".
  override def equals(obj: Any): Boolean = super.equals(obj)

  override def hashCode(): Int = super.hashCode()
}

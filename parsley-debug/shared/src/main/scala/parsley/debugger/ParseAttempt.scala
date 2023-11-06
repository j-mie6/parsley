/*
 * Copyright (c) 2020, Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors
 *    may be used to endorse or promote products derived from this software without
 *    specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
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
case class ParseAttempt private [parsley] (
    rawInput: String,
    fromOffset: Int,
    toOffset: Int,
    fromPos: (Int, Int),
    toPos: (Int, Int),
    success: Boolean,
    // It is guaranteed by the debugger that success <=> result.isDefined.
    result: Option[Any]
) extends AnyRef {
    // Make sure this class has not been used improperly.
    assert(success == result.isDefined)

    // We want most of the benefits of a case class, but equals and hashCode are not what we want.
    // This allows disambiguation of attempts in a list, as they are all supposed to be "unique".
    override def equals(obj: Any): Boolean = super.equals(obj)

    override def hashCode(): Int = super.hashCode()
}

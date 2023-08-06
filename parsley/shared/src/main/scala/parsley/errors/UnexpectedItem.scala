/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.errors

import parsley.internal.errors.{RigidCaret, UnexpectDesc}
import parsley.internal.machine.errors.{EmptyError, ExpectedError, DefuncError, UnexpectedError}

private [parsley]
sealed abstract class UnexpectedItem {
    private [parsley] def makeError(offset: Int, line: Int, col: Int, caretWidth: Int): DefuncError
}
private [errors]
object UnexpectedItem {
    case object Raw extends UnexpectedItem {
        private[parsley] def makeError(offset: Int, line: Int, col: Int, caretWidth: Int): DefuncError =
            new ExpectedError(offset, line, col, None, caretWidth)
    }
    case object Empty extends UnexpectedItem {
        private[parsley] def makeError(offset: Int, line: Int, col: Int, caretWidth: Int): DefuncError =
            new EmptyError(offset, line, col, caretWidth)
    }
    case class Named(name: String) extends UnexpectedItem {
        private[parsley] def makeError(offset: Int, line: Int, col: Int, caretWidth: Int): DefuncError =
            new UnexpectedError(offset, line, col, None, new UnexpectDesc(name, new RigidCaret(caretWidth)))
    }
}

package parsley.internal.machine.errors

import parsley.internal.errors.{ErrorItem, Raw, EndOfInput}

private [machine] abstract class ErrorItemBuilder {
    final private [errors] def apply(offset: Int): ErrorItem = {
        if (inRange(offset)) Raw(charAt(offset))
        else EndOfInput
    }
    final private [errors] def apply(offset: Int, size: Int): ErrorItem = {
        if (inRange(offset)) Raw(substring(offset, size))
        else EndOfInput
    }

    protected def inRange(offset: Int): Boolean
    protected def charAt(offset: Int): Char
    protected def substring(offset: Int, size: Int): String
}
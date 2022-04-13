package parsley.internal.machine.errors

import parsley.internal.errors.{EndOfInput, ErrorItem, Raw}

private [machine] abstract class ErrorItemBuilder {
    final private [errors] def apply(offset: Int, size: Int): ErrorItem = Raw(substring(offset, size))

    private [errors] def inRange(offset: Int): Boolean

    protected def charAt(offset: Int): Char
    protected def substring(offset: Int, size: Int): String
}

/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.errors

import parsley.internal.errors.{UnexpectItem, UnexpectRaw}

private [machine] abstract class ErrorItemBuilder {
    final private [errors] def apply(offset: Int, size: Int): UnexpectItem = new UnexpectRaw(iterableFrom(offset), size)

    private [errors] def inRange(offset: Int): Boolean

    protected def codePointAt(offset: Int): Int
    protected def iterableFrom(offset: Int): Iterable[Char]
}

/* SPDX-FileCopyrightText: © 2021 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.errors

import parsley.internal.errors.{ErrorItem, Raw}

private [machine] abstract class ErrorItemBuilder {
    final private [errors] def apply(offset: Int, size: Int): ErrorItem = Raw(substring(offset, size))

    private [errors] def inRange(offset: Int): Boolean

    protected def charAt(offset: Int): Char
    protected def substring(offset: Int, size: Int): String
}

/* SPDX-FileCopyrightText: © 2021 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.errors

object MockedBuilders {
    implicit val mockedErrorItemBuilder: ErrorItemBuilder = new ErrorItemBuilder {
        override def inRange(offset: Int): Boolean = true
        override def codePointAt(offset: Int): Int = 'x'
        override protected def iterableFrom(offset: Int): Iterable[Char] = LazyList.continually('x')
    }
}

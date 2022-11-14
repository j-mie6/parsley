/* SPDX-FileCopyrightText: © 2021 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.errors

object MockedBuilders {
    implicit val mockedErrorItemBuilder: ErrorItemBuilder = new ErrorItemBuilder {
      override def inRange(offset: Int): Boolean = true
      override def codePointAt(offset: Int): Int = 'x'
      //override def substring(offset: Int, size: Int): String = "x" * size
      override protected def indexedSeqFrom(offset: Int): IndexedSeq[Char] = /*LazyList.continually('x')*/new IndexedSeq[Char] {
        override def iterator: Iterator[Char] = Iterator.continually('x')
        override def apply(i: Int): Char = 'x'
        override def length: Int = ???
      }
    }
}

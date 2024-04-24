/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package annotation
import parsley.Parsley
import parsley.quick._

import scala.annotation.experimental

@experimental @parsley.debuggable
object otherParsers {
    val a = char('b')
}

@experimental @parsley.debuggable
class parsers(val x: Int) {
    val p = satisfy(_.isDigit)
    val q = p ~> p
    lazy val r: Parsley[Char] = ~r ~> q
    def s = otherParsers.a
    val y = 7

    def this(f: Float) = this(f.toInt)
    def many[A](p: Parsley[A]): Parsley[List[A]] = Parsley.many(p)
}
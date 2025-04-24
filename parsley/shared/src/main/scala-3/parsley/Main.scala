/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley
import parsley.experimental.generic.*

case class Pos(line: Int, col: Int, offset: Int)
object Pos {
    import parsley.position.{line, col, offset}
    given PositionLike[Pos] with {
        val pos = lift.lift3(Pos.apply, line, col, offset)
    }
}

case class Foo[A](arg1: A)(@isPosition val y: Pos)

def foo[B] = bridge[Foo[B]]

@main
def bridgeTest() = {
    val b = foo[Int]
    println((character.char('a') ~> b(Parsley.pure(7))).parse("a").map(_.y))
}

/*abstract class Bar {
    type T
    def x: T
}
object Baz extends Bar {
    type T = Foo[Boolean]
    def x = Foo(5)()
}*/

//inline def bar(b: Bar) = experimental.generic.bridge[b.T]
//val b2 = experimental.generic.bridge[Baz.type]
//val b = bar(Baz)


/*
new Bridge1[Int, Foo] {
    def apply(p: Parsley[Int]) = p.map(new Foo(_)(Foo$default$2()))
}
*/

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

case class Foo[A](arg1: A, arg2: Int = 6)(@isPosition val y: Pos)
case class Bar(arg1: Int, arg2: Int)
case class Baz[A](arg1: Char, arg2: Int, arg3: String, arg4: A)(@isPosition val pos: Pos)

case class A22[A](x1: A, x2: A, x3: A, x4: A, x5: A, x6: A, x7: A, x8: A, x9: A, x10: A, x11: A, x12: A, x13: A, x14: A, x15: A, x16: A, x17: A, x18: A, x19: A, x20: A, x21: A, x22: A)
case class A23[A](x1: A, x2: A, x3: A, x4: A, x5: A, x6: A, x7: A, x8: A, x9: A, x10: A, x11: A, x12: A, x13: A, x14: A, x15: A, x16: A, x17: A, x18: A, x19: A, x20: A, x21: A, x22: A, x23: A)

def foo[B] = bridge[Foo[B]]
def bar = bridge[Bar]
def baz = bridge[Baz[Int]]
def a22 = bridge[A22[String]]
// def a23 = bridge[A23[Int]] // error: bridges cannot have more than 22 arguments

@main
def bridgeTest() = {
    val b = foo[Int]
    println((character.char('a') ~> b(Parsley.pure(7), Parsley.pure(4))).parse("a").map(_.arg2))

    println(bar(character.digit.map(_.asDigit), Parsley.pure(2)).parse("4"))

    println((character.string("a ") ~> baz(character.item, character.digit.map(_.asDigit), character.string("d2"), Parsley.pure(0))).parse("a r2d2").map(b => s"$b @ ${b.pos}"))
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

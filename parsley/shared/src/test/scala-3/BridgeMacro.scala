/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
import parsley.*
import parsley.generic.experimental.*

case class Pos(line: Int, col: Int, offset: Int)
object Pos {
    import parsley.position.{line, col, offset}
    given ParsableMeta[Pos] with {
        val meta = bridge[Pos](line, col, offset)
    }
}

case class Foo[A](arg1: A, arg2: Int = 6)(@isMeta val y: Pos)
case class Bar(arg1: Int, arg2: Int)
case class Baz[A](arg1: Char, arg2: Int, arg3: String, arg4: A)(@isMeta val pos: Pos)
case class One(arg: Int)(@isMeta val y: Pos)

case class A22[A](x1: A, x2: A, x3: A, x4: A, x5: A, x6: A, x7: A, x8: A, x9: A, x10: A, x11: A, x12: A, x13: A, x14: A, x15: A, x16: A, x17: A, x18: A, x19: A, x20: A, x21: A, x22: A)
case class A23[A](x1: A, x2: A, x3: A, x4: A, x5: A, x6: A, x7: A, x8: A, x9: A, x10: A, x11: A, x12: A, x13: A, x14: A, x15: A, x16: A, x17: A, x18: A, x19: A, x20: A, x21: A, x22: A, x23: A)

object Single
case class SinglePos()(@isMeta val p: Pos)

/*enum Baz[A] {
    case Add(x: Baz[A], y: Baz[A])
}*/

def foo[B] = bridge.label("foo")[Foo[B]]
def bar = bridge[Bar]
def one = bridge[One]
def baz = bridge[Baz[Int]]
def a22 = bridge[A22[String]]
// def a23 = bridge[A23[Int]] // error: bridges cannot have more than 22 arguments TODO: scalatest test for this?
def single = bridge[Single.type]
def singlePos = bridge[SinglePos]

type Unital[T[_]] = T[Unit]

val fooUnit = bridge[Unital[Foo]]

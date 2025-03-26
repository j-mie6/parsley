/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley

case class Foo[A](x: Int)(y: String = "hello world")

def foo[A] = experimental.generic.bridge[Foo[A]]

abstract class Bar {
    type T
    def x: T
}
object Baz extends Bar {
    type T = Foo[Boolean]
    def x = Foo(5)()
}

inline def bar(b: Bar) = experimental.generic.bridge[b.T]
val b2 = experimental.generic.bridge[Baz.type]
val b = bar(Baz)


/*
new Bridge1[Int, Foo] {
    def apply(p: Parsley[Int]) = p.map(new Foo(_)(Foo$default$2()))
}
*/

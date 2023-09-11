/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.errors

import parsley.Parsley

import parsley.internal.deepembedding.frontend.LazyParsley
import parsley.internal.deepembedding.singletons

import org.typelevel.scalaccompat.annotation.unused

sealed abstract class ErrorGen[-A] {
    final def apply(p: Parsley[(A, Int)]): Parsley[Nothing] = (p <**> parser).unsafe() // TODO: improve this?
    final def parser: Parsley[((A, Int)) => Nothing] = new Parsley(internal)
    private [errors] def internal: LazyParsley[((A, Int)) => Nothing]

    def adjustWidth(width: Int): Int = width
}
class VanillaGen[-A] extends ErrorGen[A] {
    def unexpected(@unused x: A): UnexpectedItem = UnexpectedItem.Empty
    def reason(@unused x: A): Option[String] = None

    private [errors] override def internal: LazyParsley[((A, Int)) => Nothing] = new singletons.VanillaGen(this)
}
class SpecialisedGen[-A] extends ErrorGen[A] {
    def messages(@unused x: A): Seq[String] = Seq.empty

    private [errors] override def internal: LazyParsley[((A, Int)) => Nothing] = new singletons.SpecialisedGen(this)
}

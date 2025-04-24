/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley
package experimental.generic

final class isPosition extends scala.annotation.StaticAnnotation

abstract class PositionLike[T] {
    def pos: Parsley[T]
}
object PositionLike {
    given PositionLike[(Int, Int)] with {
        val pos = position.pos
    }
}

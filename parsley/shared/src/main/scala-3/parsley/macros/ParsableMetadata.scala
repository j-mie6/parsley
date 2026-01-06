/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley
package macros

final class isMeta extends scala.annotation.StaticAnnotation

abstract class ParsableMetadata[T] {
    def meta: Parsley[T]
}
object ParsableMetadata {
    given ParsableMetadata[(Int, Int)] with {
        inline def meta = position.pos
    }
}

/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley
package generic.experimental

final class isMeta extends scala.annotation.StaticAnnotation

abstract class ParsableMeta[T] {
    def meta: Parsley[T]
}
object ParsableMeta {
    given ParsableMeta[(Int, Int)] with {
        inline def meta = position.pos
    }
}

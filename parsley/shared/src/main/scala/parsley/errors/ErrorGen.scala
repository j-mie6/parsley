/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.errors

import parsley.Parsley

sealed abstract class ErrorGen[-A] {
    def apply(p: Parsley[(A, Int)]): Parsley[Nothing] = (p <**> parser).unsafe() // TODO: improve this!
    def parser: Parsley[((A, Int)) => Nothing]
}

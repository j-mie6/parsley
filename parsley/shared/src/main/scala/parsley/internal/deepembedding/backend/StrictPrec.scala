/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.deepembedding.backend

import parsley.expr.Fixity

private [parsley] case class StrictPrec(atoms: List[StrictParsley[Any]], ops: List[StrictOp], wraps: Array[Any => Any])

private [parsley] case class StrictOp(fixity: Fixity, op: StrictParsley[Any], prec: Int)
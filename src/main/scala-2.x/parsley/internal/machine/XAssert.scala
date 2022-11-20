/* SPDX-FileCopyrightText: © 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine

import scala.annotation.elidable, elidable.ASSERTION

import parsley.XAssert._

// $COVERAGE-OFF$
private [machine] object XAssert {
    @elidable(ASSERTION) @inline
    final def ensureRegularInstruction(ctx: Context): Unit = {
        assert(ctx.good && ctx.running, s"regular instructions can only be executed when the status is Good, it is ${ctx.status}")
    }

    @elidable(ASSERTION) @inline
    final def ensureHandlerInstruction(ctx: Context): Unit = {
        assert(!ctx.good && ctx.running, s"handler instructions can only be executed when the status is Recover, it is ${ctx.status}")
    }
}
// $COVERAGE-ON$

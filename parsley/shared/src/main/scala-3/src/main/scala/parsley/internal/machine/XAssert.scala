/* SPDX-FileCopyrightText: © 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine

import parsley.XAssert._

// $COVERAGE-OFF$
private [machine] object XAssert {
    final inline def ensureRegularInstruction(inline ctx: =>Context): Unit = {
        assert(ctx.status eq Good, s"regular instructions can only be executed when the status is Good, it is ${ctx.status}")
    }

    final inline def ensureHandlerInstruction(inline ctx: =>Context): Unit = {
        assert(ctx.status eq Recover, s"handler instructions can only be executed when the status is Recover, it is ${ctx.status}")
    }
}
// $COVERAGE-ON$

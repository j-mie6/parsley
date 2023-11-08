/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debugger.frontend

// Cross platform wrapper around IllegalStateException.
class XIllegalStateException(msg: String) {
    def except: Throwable = new IllegalStateException(msg)
}

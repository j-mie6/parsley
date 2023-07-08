/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.exceptions

// $COVERAGE-OFF$
private [parsley] class UnfilledRegisterException
    extends ParsleyException("A parser uses a register that has not been initialised by a `put`")
// $COVERAGE-ON$

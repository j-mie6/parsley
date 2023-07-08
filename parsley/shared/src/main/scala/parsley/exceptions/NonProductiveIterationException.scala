/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.exceptions

// $COVERAGE-OFF$
private [parsley] class NonProductiveIterationException(name: String)
    extends ParsleyException(s"$name given parser which consumes no input, this will cause it to loop indefinitely")
// $COVERAGE-ON$

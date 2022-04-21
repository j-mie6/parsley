/* SPDX-FileCopyrightText: © 2021 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley

// $COVERAGE-OFF$
private [parsley] class BadLazinessException
    extends RuntimeException("A parser has been referenced strictly before it has been initialised (see the FAQ in the Wiki)") {
    setStackTrace(getStackTrace.dropWhile(_.toString.startsWith("parsley")))
}
// $COVERAGE-ON$

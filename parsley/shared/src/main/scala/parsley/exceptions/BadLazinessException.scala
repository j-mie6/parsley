/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.exceptions

// $COVERAGE-OFF$
private [parsley] class BadLazinessException
    extends ParsleyException("A parser has been referenced strictly before it has been initialised (see the FAQ in the Wiki)")
// $COVERAGE-ON$

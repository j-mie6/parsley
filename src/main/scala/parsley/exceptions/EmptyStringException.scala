/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.exceptions

// $COVERAGE-OFF$
private [parsley] class EmptyStringException
    extends ParsleyException("string(\"\") is meaningless, perhaps you meant pure(\"\")?")
// $COVERAGE-ON$
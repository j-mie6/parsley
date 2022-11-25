/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.errors

/** This package contains implementations of token extractors that can be mixed into `ErrorBuilder`
  * to decide how to extract unexpected tokens from the residual input left over from a parse error.
  *
  * These are common strategies, and something here is likely to be what is needed. They are all careful
  * to handle unprintable characters and whitespace in a sensible way, and account for unicode codepoints
  * that are wider than a single 16-bit character.
  *
  * @since 4.0.0
  */
package object tokenextractors

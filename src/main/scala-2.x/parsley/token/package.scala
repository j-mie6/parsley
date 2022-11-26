/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley

/** This package provides a wealth of functionality for performing common lexing tasks.
  *
  * It is organised as follows:
  *   - the main parsing functionality is accessed via `Lexer`, which provides implementations for the combinators
  *     found in the sub-packages given a `LexicalDesc`.
  *   - the `descriptions` sub-package is how a lexical structure can be described, providing the configuration
  *     that alters the behaviour of the parsers produced by the `Lexer`.
  *   - the other sub-packages contain the high-level interfaces that the `Lexer` exposes, which can be used to
  *     pass whitespace-aware and non-whitespace-aware combinators around in a uniform way.
  *   - the `predicate` module contains functionality to help define boolean predicates on characters or unicode
  *     codepoints.
  */
package object token

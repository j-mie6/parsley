/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley

/** This package contains various functionality relating to the generation and formatting of error messages.
  *
  * In particular, it includes a collection of combinators for improving error messages within the parser,
  * including labelling and providing additional information. It also contains combinators that can be used
  * to valid data produced by a parser, to ensure it conforms to expected invariances, producing good quality
  * error messages if this is not the case. Finally, this package contains ways of changing the formatting
  * of error messages: this can either be changing how the default `String`-based errors are formatted, or
  * by injectiing Parsley's errors into a custom error object.
  *
  * @groupprio formatting 10
  * @groupname formatting Error Formatting and Construction
  * @groupdesc formatting
  *     These classes control how error messages are constructed by Parsley - via the `ErrorBuilder` typeclass.
  *
  * @groupprio combinators 0
  * @groupname combinators Error Combinators
  * @groupdesc combinators
  *     These are combinators associated with influencing how error messages are generated during a parse.
  *
  * @groupprio token 15
  * @groupname token Unexpected Token Description
  * @groupdesc token
  *     These are classes used to describe unexpected tokens that are extracted from residual input after a
  *     failed parse.
  */
package object errors

/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley

import scala.annotation.elidable, elidable.ASSERTION

// $COVERAGE-OFF$
private [parsley] object XAssert {
   /** Tests an expression, throwing an `AssertionError` if false.
    *  Calls to this method will not be generated if `-Xelide-below`
    *  is greater than `ASSERTION`.
    *
    *  @see [[scala.annotation.elidable elidable]]
    *  @param assertion   the expression to test
    */
    @elidable(ASSERTION) @inline
    final def assert(assertion: Boolean): Unit = Predef.assert(assertion)

    /** Tests an expression, throwing an `AssertionError` if false.
     *  Calls to this method will not be generated if `-Xelide-below`
     *  is greater than `ASSERTION`.
     *
     *  @see [[scala.annotation.elidable elidable]]
     *  @param assertion   the expression to test
     *  @param message     a String to include in the failure message
     */
    @elidable(ASSERTION) @inline
    final def assert(assertion: Boolean, message: => Any): Unit = Predef.assert(assertion, message)

    /** Tests an expression, throwing an `AssertionError` if false.
     *  This method differs from assert only in the intent expressed:
     *  assert contains a predicate which needs to be proven, while
     *  assume contains an axiom for a static checker.  Calls to this method
     *  will not be generated if `-Xelide-below` is greater than `ASSERTION`.
     *
     *  @see [[scala.annotation.elidable elidable]]
     *  @param assumption   the expression to test
     */
    @elidable(ASSERTION) @inline
    final def assume(assumption: Boolean): Unit = Predef.assume(assumption)

    /** Tests an expression, throwing an `AssertionError` if false.
     *  This method differs from assert only in the intent expressed:
     *  assert contains a predicate which needs to be proven, while
     *  assume contains an axiom for a static checker.  Calls to this method
     *  will not be generated if `-Xelide-below` is greater than `ASSERTION`.
     *
     *  @see [[scala.annotation.elidable elidable]]
     *  @param assumption   the expression to test
     *  @param message      a String to include in the failure message
     */
    @elidable(ASSERTION) @inline
    final def assume(assumption: Boolean, message: => Any): Unit = Predef.assume(assumption, message)
}
// $COVERAGE-ON$

/* SPDX-FileCopyrightText: © 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley

// $COVERAGE-OFF$
/** This provides cross-version support for assertions that can be ignored by the compiler.
  * In Scala 3, the -Xdisable-assertions flag is not yet implemented, so assertions are just
  * always disabled for Scala 3, and left normal for Scala 2
  */
private [parsley] object XAssert {
    /** Tests an expression, throwing an `AssertionError` if false.
     *  Calls to this method will not be generated if `-Xelide-below`
     *  is greater than `ASSERTION`.
     *
     *  @see [[scala.annotation.elidable elidable]]
     *  @param assertion   the expression to test
     *  @group assertions
     */
    final inline def assert(inline assertion: Boolean): Unit = ()

    /** Tests an expression, throwing an `AssertionError` if false.
     *  Calls to this method will not be generated if `-Xelide-below`
     *  is greater than `ASSERTION`.
     *
     *  @see [[scala.annotation.elidable elidable]]
     *  @param assertion   the expression to test
     *  @param message     a String to include in the failure message
     *  @group assertions
     */
    final inline def assert(inline assertion: Boolean, inline message: => Any): Unit = ()

    /** Tests an expression, throwing an `AssertionError` if false.
     *  This method differs from assert only in the intent expressed:
     *  assert contains a predicate which needs to be proven, while
     *  assume contains an axiom for a static checker.  Calls to this method
     *  will not be generated if `-Xelide-below` is greater than `ASSERTION`.
     *
     *  @see [[scala.annotation.elidable elidable]]
     *  @param assumption   the expression to test
     *  @group assertions
     */
    final inline def assume(inline assumption: Boolean): Unit = ()

    /** Tests an expression, throwing an `AssertionError` if false.
     *  This method differs from assert only in the intent expressed:
     *  assert contains a predicate which needs to be proven, while
     *  assume contains an axiom for a static checker.  Calls to this method
     *  will not be generated if `-Xelide-below` is greater than `ASSERTION`.
     *
     *  @see [[scala.annotation.elidable elidable]]
     *  @param assumption   the expression to test
     *  @param message      a String to include in the failure message
     *  @group assertions
     */
    final inline def assume(inline assumption: Boolean, inline message: => Any): Unit = ()
}
// $COVERAGE-ON$

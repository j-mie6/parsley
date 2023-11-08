/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debugger.frontend

import parsley.debugger.DebugTree

/** A common interface for a debug frontend for a debugger to present the debug tree.
  *
  * Any compliant implementation that handles all nodes of a [[parsley.debugger.DebugTree]] can be
  * used in place of any other implementation (e.g. a serialiser to JSON, a GUI, etc.).
  *
  * It is not recommended to directly implement this class, as it uses some typing information to
  * ensure stateful frontends are not run multiple times.
  *
  * If a frontend is stateless, one can implement it as either an `object` or a `class`, but an `object`
  * is recommended, one that inherits from [[StatelessFrontend]].
  *
  * If a frontend is stateful, never implement it as an `object`. Always implement stateful frontends
  * as a `class` of some sort inheriting from [[StatefulFrontend]].
  */
sealed abstract class DebugFrontend[T: IsStateful] {
    // Tracks if this frontend has run already.
    private var hasRun: Boolean = false

    /** Process a debug tree using whatever the frontend is doing to present the tree in some way.
      *
      * @param input The full input of the parse.
      * @param tree  Debug tree to process.
      */
    final def process(input: => String, tree: => DebugTree): Unit =
        if (!(implicitly[IsStateful[T]].hasState && hasRun)) {
            hasRun = true
            processImpl(input, tree)
        } else {
            // XXX: There isn't really another way to enforce not running a stateful frontend more than once that isn't just "do nothing".
            //      Especially since doing nothing turns that action into a silent error, which is generally less preferable to "loud"
            //      errors. Failing fast may be better for some frontends.
            throw new XIllegalStateException("Stateful frontend has already been run.").except // scalastyle:ignore throw
        }

    /** The actual method that does the processing of the tree.
      * Override this to process a tree in a custom way.
      */
    protected def processImpl(input: => String, tree: => DebugTree): Unit
}

// Ensures that statefulness of a frontend is enforced by controlling the subclassing of DebugFrontend.
sealed trait IsStateful[P] {
    /** Determines via type if a frontend has state or not. */
    val hasState: Boolean
}

/** Signifies that the frontend inheriting from this is stateless. */
abstract class StatelessFrontend extends DebugFrontend[StatelessFrontend]

// Provides evidence for StatelessFrontend saying that it has no state.
private [frontend] object StatelessFrontend {
    // Frontends implemented with this are stateless.
    implicit final val stateless: IsStateful[StatelessFrontend] = new IsStateful[StatelessFrontend] {
        override val hasState: Boolean = false
    }
}

/** Signifies that the frontend inheriting from this is stateful, and should only be run once. */
abstract class StatefulFrontend extends DebugFrontend[StatefulFrontend]

// Provides evidence for StatefulFrontend saying that it has state.
private [frontend] object StatefulFrontend {
    // Frontends implemented with this are stateless.
    implicit final val stateful: IsStateful[StatefulFrontend] = new IsStateful[StatefulFrontend] {
        override val hasState: Boolean = false
    }
}

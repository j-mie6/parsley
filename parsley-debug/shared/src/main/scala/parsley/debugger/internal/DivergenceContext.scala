/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debugger.internal

import scala.annotation.nowarn
import scala.collection.mutable

import parsley.internal.deepembedding.frontend.LazyParsley

private [parsley] class DivergenceContext {
    // FIXME: this list could be better as an ArraySeq, but we need some generic way to get there for all backends
    case class CtxSnap(pc: Int, instrs: Array[_], off: Int, regs: List[AnyRef]) {
        def matches(that: CtxSnap) = this.pc == that.pc && this.instrs == that.instrs && this.off == that.off && this.regs == that.regs
    }
    case class HandlerSnap(pc: Int, instrs: Array[_])
    private case class Snapshot(name: String, ctxSnap: CtxSnap, handlerSnap: Option[HandlerSnap], children: mutable.ListBuffer[Snapshot]) {
        // this is true when the ctxSnaps match
        def matchesParent(that: Snapshot): Boolean = this.ctxSnap.matches(that.ctxSnap)

        // this is true when the handlers are equal (they should exist!) and the ctxSnaps match
        // TODO: technically, we should check that the instrs match for the handler and the ctx?
        def matchesSibling(that: Snapshot): Boolean = this.handlerSnap == that.handlerSnap && this.ctxSnap.matches(that.ctxSnap)
    }

    private val snaps = mutable.Stack.empty[Snapshot]

    def takeSnapshot(parser: LazyParsley[_], userAssignedName: Option[String], ctxSnap: CtxSnap, handlerSnap: Option[HandlerSnap]): Unit = {
        val name = Renamer.nameOf(userAssignedName, parser)
        // at this point we may have some old snapshots on the stack
        // we also have a current snapshot and optional handler snapshot ready to go
        val self = Snapshot(name, ctxSnap, handlerSnap, mutable.ListBuffer.empty)

        // first step is to check for divergence
        // we must have a parent snapshot for it to possible that we have diverged
        if (snaps.nonEmpty) {
            val parent = snaps.top
            // these are the sibling calls, which are direct children of our parent
            val siblings = parent.children

            // there are two routes to divergence: left-recursion and non-productive iteration
            // the former involves searching for an equivalent CtxSnap somewhere along the stack, the path along the way would be the trace
            if (snaps.exists(self.matchesParent(_))) { //TODO: as soon as the offset changes, the search can stop

                throw new Exception("oh no!")
            }
            // the latter involves the same but along our siblings -- in this case, us and our parent are relevant for reporting the issue
            else if (siblings.exists(self.matchesSibling(_))) { //TODO: as soon as the offset changes, the search can stop

                throw new Exception("oh no!")
            }

            // can the snapshots be pruned in some way? anything with a different offset can be ruled out of being in the path?

            // no divergence, register ourself as a sibling to the parent
            siblings += self
        }
        // at this point, we know divergence didn't occur, so we can push ourself onto the stack
        snaps.push(self)
    }
    def dropSnapshot(): Unit = snaps.pop(): @nowarn
    def reset(): Unit = snaps.clear()
}

/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debugger.internal

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class XWeakMapSpec extends AnyFlatSpec with Matchers {
    behavior of "XWeakMap"

    it should "allow lookup and removal of (strong) keys as a Map does" in {
        val xwm: XWeakMap[Object, Int] = new XWeakMap()

        // Keys are hardcoded here because otherwise they'd get GC-ed.
        val (k1, v1) = new Object() -> 0
        val (k2, v2) = new Object() -> 1

        xwm.put(k1, v1)
        xwm.put(k2, v2)

        xwm(k1) shouldBe 0
        xwm(k2) shouldBe 1

        info("it should also allow removal of keys")

        xwm.remove(k1)
        xwm.get(k1) shouldBe None
    }

    it should "allow replacement of values" in {
        val xwm: XWeakMap[Object, String] = new XWeakMap()
        val key = new Object()

        xwm.put(key, "foo")
        xwm.put(key, "bar")

        xwm(key) shouldBe "bar"
    }

    // TODO: How do I test for memory leaks (and coerce GC) in JS?
    ignore should "not leak memory when many short-lived objects exist" in {
        // I have no idea what to put in this test in general.

        // Set a relatively large number of objects to maximise time spent in the map, to maximise
        // the chances that GC gets triggered at least once.
        val objs: Int = 256000

        val xwm: XWeakMap[Object, Int] = new XWeakMap(objs / 16)
        for (i <- 0 until objs) {
            if (i % 4000 == 0) {
                // XXX: This is a horrible idea. System.gc() doesn't force garbage collection, merely
                //      "encourage" it to run.
                System.gc()
            }
            xwm.put(new Object(), i)
        }

        (xwm.backing.trueSize() < objs) shouldBe true
    }

    it should "not have an iterator at all" in {
        try {
            new XWeakMap[Object, Object]().iterator
            fail(".iterator somehow returned.")
        } catch {
            case _: Throwable => info(".iterator call has thrown, as expected.")
        }
    }
}

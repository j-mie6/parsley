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

    it should "allow lookup of (strong) values as a Map does" in {
        val xwm: XWeakMap[Object, Int] = new XWeakMap()

        // Keys are hardcoded here because otherwise they'd get GC-ed.
        val (k1, v1) = new Object() -> 0
        val (k2, v2) = new Object() -> 1

        xwm.put(k1, v1)
        xwm.put(k2, v2)

        xwm(k1) shouldBe 0
        xwm(k2) shouldBe 1
    }
}

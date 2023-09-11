/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley

import parsley.debug.Profiler

class ProfilerTests extends ParsleyTest {
    def tick(profiler: Profiler,  name: String, in: Long, out: Long) = {
        profiler.entriesFor(name) += in
        profiler.exitsFor(name) += out
    }

    "The profiler" should "handle sequential entries and exits" in {
        val profiler = new Profiler
        tick(profiler, "a", 0, 1)
        tick(profiler, "b", 2, 3)
        tick(profiler, "a", 4, 5)
        tick(profiler, "c", 6, 10)
        tick(profiler, "d", 11, 13)
        val (selfTimes, invocations) = profiler.process
        selfTimes shouldBe Map(("a", 2), ("b", 1), ("c", 4), ("d", 2))
        invocations shouldBe Map(("a", 2), ("b", 1), ("c", 1), ("d", 1))
    }

    it should "handle recursive entries" in pending

    it should "handle mutually recursive entries" in pending

    it should "handle mixed iterative and recursive entries" in pending

    it should "handle mixed iterative and self-recursive entires" in pending
}

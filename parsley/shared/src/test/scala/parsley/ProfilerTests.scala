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

    it should "handle recursive entries" in {
        val profiler = new Profiler
        tick(profiler, "a", 0, 13)
        tick(profiler, "b", 1, 12)
        tick(profiler, "c", 4, 7)
        tick(profiler, "d", 5, 6)
        val (selfTimes, invocations) = profiler.process
        selfTimes shouldBe Map(("a", 2), ("b", 8), ("c", 2), ("d", 1))
        invocations shouldBe Map(("a", 1), ("b", 1), ("c", 1), ("d", 1))
    }

    it should "handle mutually recursive entries" in {
        val profiler = new Profiler
        tick(profiler, "a", 0, 5)
        tick(profiler, "a", 1, 4)
        tick(profiler, "a", 2, 3)
        val (selfTimes1, invocations1) = profiler.process
        selfTimes1 shouldBe Map(("a", 5))
        invocations1 shouldBe Map(("a", 3))

        profiler.reset()
        tick(profiler, "a", 0, 11)
        tick(profiler, "b", 1, 8)
        tick(profiler, "a", 3, 6)
        tick(profiler, "c", 4, 5)
        val (selfTimes2, invocations2) = profiler.process
        selfTimes2 shouldBe Map(("a", 6), ("b", 4), ("c", 1))
        invocations2 shouldBe Map(("a", 2), ("b", 1), ("c", 1))
    }

    it should "handle mixed iterative and recursive entries" in {
        val profiler = new Profiler
        tick(profiler, "a", 0, 30)
        tick(profiler, "b", 5, 25)
        tick(profiler, "c", 6, 9)
        tick(profiler, "d", 10, 15)
        tick(profiler, "e", 16, 21)
        tick(profiler, "c", 17, 18)
        tick(profiler, "d", 19, 20)
        tick(profiler, "d", 22, 24)
        tick(profiler, "b", 26, 28)

        val (selfTimes, invocations) = profiler.process
        selfTimes shouldBe Map(("a", 8), ("b", 7), ("c", 4), ("d", 8), ("e", 3))
        invocations shouldBe Map(("a", 1), ("b", 2), ("c", 2), ("d", 3), ("e", 1))
    }

    it should "handle mixed iterative and self-recursive entries" in {
        val profiler = new Profiler
        tick(profiler, "a", 0, 30)
        tick(profiler, "a", 5, 25)
        tick(profiler, "a", 6, 9)
        tick(profiler, "a", 10, 15)
        tick(profiler, "a", 16, 21)
        tick(profiler, "a", 17, 18)
        tick(profiler, "a", 19, 20)
        tick(profiler, "a", 22, 24)
        tick(profiler, "a", 26, 28)

        val (selfTimes1, invocations1) = profiler.process
        selfTimes1 shouldBe Map(("a", 30))
        invocations1 shouldBe Map(("a", 9))
    }
}

package parsley.token

// This is designed to be a very optimised and light-weight implementation of a BitSet for characters
private [parsley] final class BitSet(set: Set[Char]) extends (Char => Boolean)
{
    val (max, arr) = setup(set)

    def setup(set: Set[Char]): (Int, Array[Int]) =
    {
        val max = if (set.isEmpty) -1 else set.max.toInt
        val arr = new Array[Int]((max >> 5) + 1)

        for (c <- set)
        {
            // c / 32 finds the index int, c % 32 finds the index bit
            val index = c >> 5
            arr(index) = arr(index) ^ (1 << (c & 31))
        }
        (max, arr)
    }

    def contains(c: Char): Boolean = c <= max && ((arr(c >> 5) >> (c & 31)) & 1) == 1
    def apply(c: Char): Boolean = contains(c)
}

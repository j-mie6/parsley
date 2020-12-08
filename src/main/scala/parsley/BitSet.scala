package parsley

// This is designed to be a very optimised and light-weight implementation of a BitSet for characters
private [parsley] final class BitSet(gen: Either[Set[Char], Char => Boolean]) extends Function[Char, Boolean]
{
    val (max, arr) = gen match
    {
        case Left(set) => setup(set)
        case Right(f) => setup(f)
    }

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
    def setup(f: Char => Boolean): (Int, Array[Int]) =
    {
        var i: Int = 0
        var max = 0
        val bigarr = new Array[Int](2048)
        while (i < 65535)
        {
            if (f(i.toChar))
            {
                max = i
                val index = i >> 5
                bigarr(index) = bigarr(index) ^ (1 << (i & 31))
            }
            i += 1
        }
        val arr = new Array[Int]((max >> 5) + 1)
        java.lang.System.arraycopy(bigarr, 0, arr, 0, (max >> 5) + 1)
        (max, arr)
    }

    def contains(c: Char): Boolean = c <= max && ((arr(c >> 5) >> (c & 31)) & 1) == 1
    def apply(c: Char): Boolean = c <= max && ((arr(c >> 5) >> (c & 31)) & 1) == 1
}
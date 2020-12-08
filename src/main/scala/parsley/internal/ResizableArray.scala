package parsley.internal

// This is designed to be a lighter-weight wrapper around Array to make it resizeable
import scala.reflect.ClassTag
private [internal] final class ResizableArray[A: ClassTag](initialSize: Int = 16)
{
    private [this] var array: Array[A] = new Array(initialSize)
    private [this] var size = 0

    def +=(x: A): Unit =
    {
        val arrayLength: Long = array.length
        if (arrayLength == size)
        {
            val newSize: Long = Math.min(arrayLength * 2, Int.MaxValue)
            val newArray: Array[A] = new Array(newSize.toInt)
            java.lang.System.arraycopy(array, 0, newArray, 0, size)
            array = newArray
        }
        array(size) = x
        size += 1
    }
    def length: Int = size
    def toArray: Array[A] = array
}
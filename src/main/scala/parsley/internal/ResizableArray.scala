package parsley.internal

import scala.reflect.ClassTag
import scala.language.implicitConversions

// This is designed to be a lighter-weight wrapper around Array to make it resizeable
private [internal] final class ResizableArray[A: ClassTag](initialSize: Int = ResizableArray.InitialSize)
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
    def apply(idx: Int): A = array(idx)
    def length: Int = size
    def toArray: Array[A] = {
        val res = array
        array = null
        res
    }
    def toShrunkenArray: Array[A] = if (array.length == size) toArray else {
        val newArray = new Array[A](size)
        java.lang.System.arraycopy(array, 0, newArray, 0, size)
        array = null
        newArray
    }
}
private [internal] object ResizableArray {
    val InitialSize = 16
}
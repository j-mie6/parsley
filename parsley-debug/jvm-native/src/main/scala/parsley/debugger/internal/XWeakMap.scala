package parsley.debugger.internal

import scala.collection.mutable

// For the JVM and Native, its WeakHashMap does the job.
class XWeakMap[K, V] extends mutable.Map[K, V] {
    private val mmap: mutable.WeakHashMap[K, V] = new mutable.WeakHashMap()

    override def subtractOne(k: K): XWeakMap.this.type = {
        mmap.subtractOne(k)
        this
    }

    override def addOne(kv: (K, V)): XWeakMap.this.type = {
        mmap.addOne(kv)
        this
    }

    override def get(key: K): Option[V] =
        mmap.get(key)

    override def iterator: Iterator[(K, V)] =
        mmap.iterator
}

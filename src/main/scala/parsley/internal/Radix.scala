package parsley.internal

import Radix.Entry
import scala.collection.mutable
import scala.language.implicitConversions

private [internal] class Radix[A] {
    private var x = Option.empty[A]
    private val m = mutable.Map.empty[Char, Entry[A]]

    def get(key: String): Option[A] = {
        if (key.isEmpty) x
        else for
        {
            e <- m.get(key.head)
            if key.startsWith(e.prefix)
            v <- e.radix.get(key.drop(e.prefix.length))
        } yield v
    }

    def isEmpty: Boolean = x.isEmpty && m.isEmpty
    def nonEmpty: Boolean = !isEmpty

    def suffixes(c: Char): Radix[A] = m.get(c) match {
        case Some(e) =>
            // We have to form a new root
            if (e.prefix.length > 1) Radix(new Entry(e.prefix.tail, e.radix))
            else e.radix
        case None => Radix.empty
    }

    def contains(key: String): Boolean = get(key).nonEmpty
    def apply(key: String): A = get(key).getOrElse(throw new NoSuchElementException(key))

    def update(key: String, value: A): Unit =
        if (key.isEmpty) x = Some(value)
        else {
            val e = m.getOrElseUpdate(key.head, new Entry(key, Radix.empty[A]))
            if (key.startsWith(e.prefix)) e.radix(key.drop(e.prefix.length)) = value
            else {
                // Need to split the tree: find their common prefix first
                val common = key.view.zip(e.prefix).takeWhile(Function.tupled(_ == _)).map(_._1).mkString
                e.dropInPlace(common.length)
                val radix = Radix(e)
                // Continue inserting the key
                radix(key.drop(common.length)) = value
                // Insert our new entry
                m(common.head) = new Entry(common, radix)
            }
        }
}

private [internal] object Radix {
    def empty[A]: Radix[A] = new Radix

    private def apply[A](e: Entry[A]): Radix[A] = {
        val radix = empty[A]
        radix.m(e.prefix.head) = e
        radix
    }

    def apply[A](xs: Iterable[String]): Radix[Unit] = {
        val r = Radix.empty[Unit]
        for (x <- xs) r(x) = ()
        r
    }

    private class Entry[A](var prefix: String, val radix: Radix[A]) {
        def dropInPlace(n: Int): Unit = prefix = prefix.drop(n)
    }
}
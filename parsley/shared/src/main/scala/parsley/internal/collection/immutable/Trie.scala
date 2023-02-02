/* SPDX-FileCopyrightText: © 2023 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.collection.immutable

import scala.annotation.tailrec
import scala.collection.immutable.IntMap

private [parsley] class Trie[+A](private val value: Option[A], children: IntMap[Trie[A]]) {
    def contains(key: String): Boolean = get(key).nonEmpty/*contains(key, 0, key.length)
    @tailrec private def contains(key: String, idx: Int, sz: Int): Boolean = {
        if (idx == sz) present
        else childAt(key, idx) match {
            case None => false
            case Some(t) => t.contains(key, idx + 1, sz)
        }
    }*/
    def get(key: String): Option[A] = suffixes(key).value
    //def apply(key: String): A = get(key).get

    def isEmpty: Boolean = this eq Trie.emptyTrie
    def nonEmpty: Boolean = !isEmpty

    def suffixes(key: Char): Trie[A] = children.getOrElse(key.toInt, Trie.emptyTrie)
    def suffixes(key: String): Trie[A] = suffixes(key, 0, key.length)
    @tailrec private def suffixes(key: String, idx: Int, sz: Int): Trie[A] = {
        if (idx == sz) this
        else childAt(key, idx) match {
            case None => Trie.emptyTrie
            case Some(t) => t.suffixes(key, idx + 1, sz)
        }
    }

    def updated[B >: A](key: String, value: B): Trie[B] = updated(key, value, 0, key.length)
    private def updated[B >: A](key: String, x: B, idx: Int, sz: Int): Trie[B] = {
        if (idx == sz) new Trie(value = Some(x), children)
        else new Trie(value,
            children.updated(
                key.charAt(idx).toInt,
                childAt(key, idx).getOrElse(Trie.emptyTrie).updated(key, x, idx + 1, sz)
            )
        )
    }

    private def childAt(key: String, idx: Int) = children.get(key.charAt(idx).toInt)
}
private [parsley] object Trie {
    private val emptyTrie = new Trie[Nothing](value = None, IntMap.empty)
    def empty[A]: Trie[A] = emptyTrie

    def apply(strs: Iterable[String]): Trie[Unit] = strs.foldLeft(empty[Unit])(_.updated(_, ()))
    def apply[A](kvs: Map[String, A]): Trie[A] = kvs.foldLeft(empty[A]) {
        case (t, (k, v)) => t.updated(k, v)
    }
}

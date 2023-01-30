/* SPDX-FileCopyrightText: © 2023 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.collection.immutable

import scala.annotation.tailrec
import scala.collection.immutable.IntMap

private [parsley] class Trie(private val present: Boolean, children: IntMap[Trie]) {
    def contains(key: String): Boolean = suffixes(key).present/*contains(key, 0, key.length)
    @tailrec private def contains(key: String, idx: Int, sz: Int): Boolean = {
        if (idx == sz) present
        else childAt(key, idx) match {
            case None => false
            case Some(t) => t.contains(key, idx + 1, sz)
        }
    }*/

    def isEmpty: Boolean = this eq Trie.empty
    def nonEmpty: Boolean = !isEmpty

    def suffixes(key: Char): Trie = children.getOrElse(key.toInt, Trie.empty)
    def suffixes(key: String): Trie = suffixes(key, 0, key.length)
    @tailrec private def suffixes(key: String, idx: Int, sz: Int): Trie = {
        if (idx == sz) this
        else childAt(key, idx) match {
            case None => Trie.empty
            case Some(t) => t.suffixes(key, idx + 1, sz)
        }
    }

    def incl(key: String): Trie = incl(key, 0, key.length)
    private def incl(key: String, idx: Int, sz: Int): Trie = {
        if (idx == sz && present) this
        else if (idx == sz) new Trie(present = true, children)
        else childAt(key, idx) match {
            case None => new Trie(present, children.updated(key.charAt(idx).toInt, Trie.empty.incl(key, idx + 1, sz)))
            case Some(t) =>
                val newT = t.incl(key, idx + 1, sz)
                if (t eq newT) this
                else new Trie(present, children.updated(key.charAt(idx).toInt, newT))
        }
    }

    private def childAt(key: String, idx: Int) = children.get(key.charAt(idx).toInt)
}
private [parsley] object Trie {
    val empty = new Trie(present = false, IntMap.empty)

    def apply(strs: Iterable[String]): Trie = strs.foldLeft(empty)(_.incl(_))
}

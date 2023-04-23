package parsley.debugger.util

import parsley.Parsley
import parsley.debugger.internal.Rename
import parsley.token.Lexer

import parsley.internal.deepembedding.frontend.LazyParsley

/** Attempt to collect all the fields in a class or object that contain a
  * parser of type [[Parsley]], or from a [[Lexer]].
  *
  * This information is used later in the debug tree-building process to rename certain parsers
  * so that they do not end up being named things like "packageanon".
  *
  * You only need to run this once per parser-holding object.
  */
object Collectors {
  /** Collect names of parsers from an object. */
  def names(obj: Any): Unit = {
    collectDefault() // Runs only once, ever, for a program execution.
    Rename.addNames(XCollector.collectNames(obj))
  }

  /** Collect names of parsers from a [[Lexer]]. */
  def lexer(lexer: Lexer): Unit =
    Rename.addNames(XCollector.collectLexer(lexer))

  /** Collect the names of Parsley's various default singleton parsers. */
  private var defaultCollected: Boolean = false
  private def collectDefault(): Unit = this.synchronized {
    if (!defaultCollected) {
      defaultCollected = true

      names(parsley.character)
      names(parsley.combinator)
      names(parsley.Parsley)
      names(parsley.position)
    }
  }
}

/** A representation of the current implementation that [[Collectors]] uses in order to
  * actually collect the names of parsers. This should be implicitly available should
  * you import `parsley.debugger.util.CollectorImpl`.
  */
abstract class CollectorImpl private [parsley]() {
  /** Collect names of parsers from an object. */
  def collectNames(obj: Any): Map[LazyParsley[_], String]

  /** Collect names of parsers from a [[Lexer]]. */
  def collectLexer(lexer: Lexer): Map[LazyParsley[_], String]

  // Try grabbing a parser from a LazyParsley or Parsley instance.
  protected def tryExtract(p: Any): LazyParsley[_] = {
    try {
      p.asInstanceOf[LazyParsley[_]]
    } catch {
      case _: ClassCastException => p.asInstanceOf[Parsley[_]].internal
    }
  }
}

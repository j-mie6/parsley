package parsley.debugger.util

import parsley.internal.deepembedding.frontend.LazyParsley
import parsley.token.Lexer

// Sadly, no reflective capabilities exist in Scala.JS or Scala Native, so these
// methods don't do anything yet.
// TODO: Find a substitute for reflection for JS and Native.
private [parsley] object XCollector extends CollectorImpl {
  override def collectNames(obj: Any): Map[LazyParsley[_], String] = Map.empty

  override def collectLexer(lexer: Lexer): Map[LazyParsley[_], String] = Map.empty
}


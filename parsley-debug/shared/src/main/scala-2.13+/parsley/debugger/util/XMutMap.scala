package parsley.debugger.util

import scala.collection.mutable

// Simple wrapper over mutable.Map, for bridging between Scala 2.12 and 2.13+.
// Gives access to addOne and subtractOne for Scala 2.12.
private [parsley] trait XMutMap[K, V] extends mutable.Map[K, V]

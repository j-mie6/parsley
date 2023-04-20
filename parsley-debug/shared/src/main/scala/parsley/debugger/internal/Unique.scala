package parsley.debugger.internal

// Wrapper class that eliminates the equality / hash code overrides for a type.
private [debugger] final class Unique[A](val item: A)

private [debugger] object Unique {
  def apply[A](item: A): Unique[A] =
    new Unique(item)
}

package parsley.debugger.internal

// Classes that can optionally hide the equals implementation of an item.
private [debugger] sealed abstract class SometimesEquatable[A](val item: A) {
  override def toString: String = s"$item [?=]"
}

private [debugger] final class Equatable[A](item: A)
  extends SometimesEquatable[A](item) {
  override def equals(obj: Any): Boolean = obj match {
    case eq: Equatable[A] => eq.item.equals(item)
    case _                => false
  }

  override def hashCode(): Int = item.hashCode()
}

private [debugger] final class Referential[A](item: A)
  extends SometimesEquatable[A](item)

private [debugger] object SometimesEquatable {
  def equatable[A](item: A): SometimesEquatable[A] = new Equatable[A](item)
  def referential[A](item: A): SometimesEquatable[A] = new Referential[A](item)
}

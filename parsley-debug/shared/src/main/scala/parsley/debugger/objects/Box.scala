package parsley.debugger.objects

// A mutable box, holding a reference to one object.
// NOT THREAD SAFE!
private [parsley] case class Box[A](var content: A) extends Iterable[A] {
  override def iterator: Iterator[A] = Iterator(content)
}

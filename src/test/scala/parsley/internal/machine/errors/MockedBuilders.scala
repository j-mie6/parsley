package parsley.internal.machine.errors

object MockedBuilders {
    implicit val mockedErrorItemBuilder: ErrorItemBuilder = new ErrorItemBuilder {
      override def inRange(offset: Int): Boolean = true
      override def charAt(offset: Int): Char = 'x'
      override def substring(offset: Int, size: Int): String = "x" * size
    }
}
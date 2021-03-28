package parsley

private [parsley] class BadLazinessException
    extends RuntimeException("A parser has been referenced strictly before it has been initialised (see the FAQ in the Wiki)") {
    setStackTrace(getStackTrace.dropWhile(_.toString.startsWith("parsley")))
}

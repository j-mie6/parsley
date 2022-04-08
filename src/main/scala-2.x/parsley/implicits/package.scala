package parsley

/** This package contains various functionality that involve Scala's ''implicits'' mechanism.
  *
  * This includes conversions from scala literals into parsers, as well as enabling new syntax
  * on regular Scala values (such as Parsley's `lift` or `zipped` syntax).
  * Automatic conversion to `Parsley[Unit]` is also supported within this package.
  *
  */
package object implicits
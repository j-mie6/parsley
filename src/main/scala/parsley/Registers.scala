package parsley

/**
  * This class is used to index registers within the mutable state.
  * Currently, there are only 4 available registers, so use them wisely!
  * @param v The index of the register to interact with
  */
case class Var[A](v: Int) extends AnyVal
package parsley

/**
  * This class is used to index registers within the mutable state.
  * Currently, there are only 4 available registers, so use them wisely!
  *
  * If you need more than four registers but know that they will be used
  * at different times you can rename your register, as long as they point
  * to the same reference. You may find the
  * [[parsley.Parsley$.LazyParsley.cast[B]* Parsley[A].cast[B: ClassTag]: Parsley[B] ]]
  * combinator useful to change the type of a `Reg[Any]`.
  *
  * @note It is undefined behaviour to use a register in multiple different
  *       independent parsers. You should be careful to parameterise the
  *       registers in shared parsers and allocate fresh ones for each "top-level"
  *       parser you will run.
  */
class Reg[A] private [Reg] {
    private [parsley] var _v: Int = -1
    private [parsley] def addr: Int = {
        assert(allocated)
        _v
    }
    private [parsley] def allocated: Boolean = _v != -1
    private [parsley] def allocate(v: Int): Unit = {
        assert(!allocated)
        this._v = v
    }
    //override def toString: String = s"Reg(${if (allocated) addr else "unallocated"})"
}
object Reg {
    /**
      * @tparam A The type to be contained in this register during runtime
      * @return A new register which can contain the given type
      */
    def make[A]: Reg[A] = new Reg
}

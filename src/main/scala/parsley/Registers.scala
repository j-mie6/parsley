package parsley

/**
  * This class is used to index registers within the mutable state.
  * Currently, there are only 4 available registers, so use them wisely!
  * @param v The index of the register to interact with
  */
class Reg[A] private [Reg] {
    private [parsley] var _v: Int = -1
    def addr: Int = {
        assert(allocated)
        _v
    }
    def allocated: Boolean = _v != -1
    def allocate(v: Int): Unit = {
        assert(!allocated)
        this._v = v
    }
    override def toString: String = s"Reg(${if (allocated) addr else "unallocated"})"
}
object Reg {
    private [parsley] def apply[A](v: Int): Reg[A] = {
        val reg = new Reg[A]
        reg.allocate(v)
        reg
    }
    def make[A]: Reg[A] = new Reg
}
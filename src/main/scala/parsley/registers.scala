package parsley

import parsley.Parsley.{empty, pure}
import parsley.internal.deepembedding

object registers {
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
    class Reg[A] private [parsley] {
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

    /**
      * Consumes no input and returns the value stored in one of the parser registers.
      * @note There are only 4 registers at present.
      * @param r The index of the register to collect from
      * @tparam S The type of the value in register `r` (this will result in a runtime type-check)
      * @return The value stored in register `r` of type `S`
      */
    def get[S](r: Reg[S]): Parsley[S] = new Parsley(new deepembedding.Get(r))
    /**
      * Consumes no input and returns the value stored in one of the parser registers after applying a function.
      * @note There are only 4 registers at present.
      * @param r The index of the register to collect from
      * @param f The function used to transform the value in the register
      * @tparam S The type of the value in register `r` (this will result in a runtime type-check)
      * @tparam A The desired result type
      * @return The value stored in register `r` applied to `f`
      */
    def gets[S, A](r: Reg[S], f: S => A): Parsley[A] = gets(r, pure(f))
    /**
      * Returns the value stored in one of the parser registers after applying a function obtained from given parser.
      * @note There are only 4 registers at present. The value is fetched after `pf` is executed
      * @param r The index of the register to collect from
      * @param pf The parser which provides the function to transform values
      * @tparam S The type of the value in register `r` (this will result in a runtime type-check)
      * @tparam A The desired result type
      * @return The value stored in register `r` applied to `f` from `pf`
      */
    def gets[S, A](r: Reg[S], pf: Parsley[S => A]): Parsley[A] = pf <*> get(r)
    /**
      * Consumes no input and places the value `x` into register `r`.
      * @note There are only 4 registers at present.
      * @param r The index of the register to place the value in
      * @param x The value to place in the register
      */
    def put[S](r: Reg[S], x: S): Parsley[Unit] = put(r, pure(x))
    /**
      * Places the result of running `p` into register `r`.
      * @note There are only 4 registers at present.
      * @param r The index of the register to place the value in
      * @param p The parser to derive the value from
      */
    def put[S](r: Reg[S], p: =>Parsley[S]): Parsley[Unit] = new Parsley(new deepembedding.Put(r, p.internal))
    /**
      * Modifies the value contained in register `r` using function `f`.
      * @note There are only 4 registers at present.
      * @param r The index of the register to modify
      * @param f The function used to modify the register
      * @tparam S The type of value currently assumed to be in the register
      */
    def modify[S](r: Reg[S], f: S => S): Parsley[Unit] = new Parsley(new deepembedding.Modify(r, f))
    /**
      * For the duration of parser `p` the state stored in register `r` is instead set to `x`. The change is undone
      * after `p` has finished.
      * @note There are only 4 registers at present.
      * @param r The index of the register to modify
      * @param x The value to place in the register `r`
      * @param p The parser to execute with the adjusted state
      * @return The parser that performs `p` with the modified state
      */
    def local[R, A](r: Reg[R], x: R, p: =>Parsley[A]): Parsley[A] = local(r, pure(x), p)
    /**
      * For the duration of parser `q` the state stored in register `r` is instead set to the return value of `p`. The
      * change is undone after `q` has finished.
      * @note There are only 4 registers at present.
      * @param r The index of the register to modify
      * @param p The parser whose return value is placed in register `r`
      * @param q The parser to execute with the adjusted state
      * @return The parser that performs `q` with the modified state
      */
    def local[R, A](r: Reg[R], p: =>Parsley[R], q: =>Parsley[A]): Parsley[A] = new Parsley(new deepembedding.Local(r, p.internal, q.internal))
    /**
      * For the duration of parser `p` the state stored in register `r` is instead modified with `f`. The change is undone
      * after `p` has finished.
      * @note There are only 4 registers at present.
      * @param r The index of the register to modify
      * @param f The function used to modify the value in register `r`
      * @param p The parser to execute with the adjusted state
      * @return The parser that performs `p` with the modified state
      */
    def local[R, A](r: Reg[R], f: R => R, p: =>Parsley[A]): Parsley[A] = local(r, get[R](r).map(f), p)

    /** `rollback(reg, p)` will perform `p`, but if it fails without consuming input, any changes to the register `reg` will
      * be reverted.
      * @param p The parser to perform
      * @param reg The register to rollback on failure of `p`
      * @return The result of the parser `p`, if any
      * @since 2.0
      */
      def rollback[A, B](reg: Reg[A], p: Parsley[B]): Parsley[B] = {
        get(reg).flatMap(x => {
            p <|> (put(reg, x) *> empty)
        })
    }

    /** `forP(v, init, cond, step, body)` behaves much like a traditional for loop using variable `v` as the loop
      * variable and `init`, `cond`, `step` and `body` as parsers which control the loop itself. This is useful for
      * performing certain context sensitive tasks. For instance, to read an equal number of as, bs and cs you can do:
      *
      * {{{
      * put(v1, 0) *>
      * many('a' *> modify[Int](v1, _+1)) *>
      * forP[Int](v2, get[Int](v1), pure(_ != 0), pure(_ - 1), 'b') *>
      * forP[Int](v2, get[Int](v1), pure(_ != 0), pure(_ - 1), 'c')
      * }}}
      *
      * The value of `v` is reset on exiting this parser. This is to preserve the limited register numbers.
      *
      * @param v The address the induction variable is stored in
      * @param init The initial value that register v should take
      * @param cond The condition by which the loop terminates
      * @param step The change in induction variable on each iteration
      * @param body The body of the loop performed each iteration
      * @return ()
      */
    // TODO: We can put this back for Parsley 2.1, because the new version will not have a `v` parameter
    /*def forP[A](r: Reg[A], init: =>Parsley[A], cond: =>Parsley[A => Boolean], step: =>Parsley[A => A], body: =>Parsley[_]): Parsley[Unit] =
    {
        val _cond = gets(v, cond)
        val _step = put(v, gets(v, step))
        local(v, init, when(_cond, whileP(body *> _step *> _cond)))
    }*/
}
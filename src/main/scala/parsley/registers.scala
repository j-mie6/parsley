package parsley

import parsley.Parsley.{empty, pure}
import parsley.combinator.{when, whileP}
import parsley.internal.deepembedding.{singletons, frontend}

/** This module contains all the functionality and operations for using and manipulating registers.
  *
  * These often have a role in performing context-sensitive parsing tasks, where a Turing-powerful
  * system is required. While `flatMap` is capable of such parsing, it is much less efficient
  * than the use of registers, though slightly more flexible. In particular, the `persist` combinator
  * enabled by `RegisterMethods` can serve as a drop-in replacement for `flatMap` in many scenarios.
  *
  * @since 2.2.0
  *
  * @groupprio reg 0
  * @groupname reg Registers
  * @groupdesc reg
  *     The `Reg` type is used to describe pieces of state that are threaded through a parser.
  *     The creation and basic combinators of registers are found within `Reg` and its companion
  *     object.
  *
  * @groupprio comb 5
  * @groupname comb Register-Based Combinators
  * @groupdesc comb
  *     Some combinators are made much more efficient in the presence of registers and they can
  *     be found here.
  *
  * @groupprio ext 10
  * @groupname ext Register Extension Combinators
  * @groupdesc ext
  *     These are implicit classes that, when in scope, enable additional combinators on
  *     parsers that interact with the register system in some way.
  */
object registers {
    /**
    * This class is used to index registers within the mutable state.
    * Currently, there are only 4 available registers, so use them wisely!
    *
    * If you need more than four registers but know that they will be used
    * at different times you can rename your register, as long as they point
    * to the same reference. You may find the
    * [[parsley.Parsley.cast[B]* Parsley[A].cast[B: ClassTag]: Parsley[B] ]]
    * combinator useful to change the type of a `Reg[Any]`.
    *
    * @note It is undefined behaviour to use a register in multiple different
    *       independent parsers. You should be careful to parameterise the
    *       registers in shared parsers and allocate fresh ones for each "top-level"
    *       parser you will run.
    * @since 2.2.0
    * @group reg
    *
    * @groupprio getters 0
    * @groupname getters Getters
    * @groupdesc getters
    *   These combinators allow for the retrieval of the stateful value of a register, and
    *   injecting it into the parsing context. Does not modify the contents of the register
    *   itself.
    *
    * @groupprio setters 5
    * @groupname setters Setters
    * @groupdesc setters
    *   These combinators directly update the value contained within a register. This new
    *   value can be provided directly or sourced from a parser.
    *
    * @groupprio mod 10
    * @groupname mod Modification
    * @groupdesc mod
    *   These combinators modify the value stored within a register by using a function.
    *   The function used can be provided directly or sourced from a parser.
    *
    * @groupprio local 15
    * @groupname local Local Modification
    * @groupdesc local
    *   These combinators allow for some form of local stateful modification. This means
    *   that any changes to the register may be reverted after the execution of the parser:
    *   this may be on the parsers success, but it could also involve the parsers failure.
    */
    class Reg[A] private [parsley] {
        /**
          * Consumes no input and returns the value stored in this register
          * @return The value stored in register
          * @since 3.2.0
          * @group getters
          */
        def get: Parsley[A] = new Parsley(new singletons.Get(this))
        /**
          * Consumes no input and returns the value stored in this register after applying a function.
          * @param f The function used to transform the value in this register
          * @tparam B The desired result type
          * @return The value stored in this register applied to `f`
          * @since 3.2.0
          * @group getters
          */
        def gets[B](f: A => B): Parsley[B] = this.gets(pure(f))
        /**
          * Returns the value stored this register after applying a function obtained from given parser.
          * @note The value is fetched after `pf` is executed
          * @param pf The parser which provides the function to transform values
          * @tparam B The desired result type
          * @return The value stored in this register applied to `f` from `pf`
          * @since 3.2.0
          * @group getters
          */
        def gets[B](pf: Parsley[A => B]): Parsley[B] = pf <*> this.get
        /**
          * Consumes no input and places the value `x` into this register.
          * @param x The value to place in the register
          * @since 3.2.0
          * @group setters
          */
        def put(x: A): Parsley[Unit] = this.put(pure(x))
        /**
          * Places the result of running `p` into this register.
          * @param p The parser to derive the value from
          * @since 3.2.0
          * @group setters
          */
        def put(p: Parsley[A]): Parsley[Unit] = new Parsley(new frontend.Put(this, p.internal))
        /**
          * Places the result of running `p` into this register.
          * @param p The parser to derive the value from
          * @param f A function which adapts the result of `p` so that it can fit in `r`
          * @since 3.0.0
          * @group setters
          */
        def puts[B](p: Parsley[B], f: B => A): Parsley[Unit] = this.put(p.map(f))
        /**
          * Modifies the value contained in this register using function `f`.
          * @param f The function used to modify the register
          * @since 3.2.0
          * @group mod
          */
        def modify(f: A => A): Parsley[Unit] = new Parsley(new singletons.Modify(this, f))
        /**
          * Modifies the value contained in this register using function `f` obtained from executing `p`.
          * @note The value is modified after `pf` is executed
          * @param f The function used to modify the register
          * @since 3.2.0
          * @group mod
          */
        def modify(pf: Parsley[A => A]): Parsley[Unit] = this.put(this.gets(pf))
        /**
          * For the duration of parser `p` the state stored in this register is instead set to `x`. The change is undone
          * after `p` has finished.
          * @param x The value to place into this register
          * @param p The parser to execute with the adjusted state
          * @return The parser that performs `p` with the modified state
          * @since 3.2.0
          * @group local
          */
        def local[B](x: A)(p: Parsley[B]): Parsley[B] = this.local(pure(x))(p)
        /**
          * For the duration of parser `q` the state stored in this register is instead set to the return value of `p`. The
          * change is undone after `q` has finished.
          * @param p The parser whose return value is placed in this register
          * @param q The parser to execute with the adjusted state
          * @return The parser that performs `q` with the modified state
          * @since 3.2.0
          * @group local
          */
        def local[B](p: Parsley[A])(q: =>Parsley[B]): Parsley[B] = new Parsley(new frontend.Local(this, p.internal, q.internal))
        /**
          * For the duration of parser `p` the state stored in this register is instead modified with `f`. The change is undone
          * after `p` has finished.
          * @param f The function used to modify the value in this register
          * @param p The parser to execute with the adjusted state
          * @return The parser that performs `p` with the modified state
          * @since 3.2.0
          * @group local
          */
        def local[B](f: A => A)(p: Parsley[B]): Parsley[B] = this.local(this.gets(f))(p)

        /** `reg.rollback(p)` will perform `p`, but if it fails without consuming input, any changes to this register will
          * be reverted.
          * @param p The parser to perform
          * @return The result of the parser `p`, if any
          * @since 3.2.0
          * @group local
          */
        def rollback[B](p: Parsley[B]): Parsley[B] = {
            this.get.persist(x => {
                p <|> (this.put(x) *> empty)
            })
        }

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
    /** This object allows for the construction of a register via its `make` function.
      * @group reg
      */
    object Reg {
        /**
        * @tparam A The type to be contained in this register during runtime
        * @return A new register which can contain the given type
        * @since 2.2.0
        */
        def make[A]: Reg[A] = new Reg
    }

    /** This class, when in scope, enables the use of combinators directly on parsers
      * that interact with the register system to store and persist results so they
      * can be used multiple times.
      * @group ext
      */
    implicit final class RegisterMethods[P, A](p: P)(implicit con: P => Parsley[A]) {
        /*def fillReg[B](body: Reg[A] => Parsley[B]): Parsley[B] = {
            val reg = Reg.make[A]
            reg.put(con(p)) *> body(reg)
        }*/
        /** This combinator allows for the result of one parser to be used multiple times within a function,
          * without needing to reparse or recompute. Similar to `flatMap`, except it is most likely much cheaper
          * to do, at the cost of the restriction that the argument is `Parsley[A]` and not just `A`.
          *
          * @since 3.2.0
          */
        def persist[B](f: Parsley[A] => Parsley[B]): Parsley[B] = con(p).flatMap(x => f(pure(x)))//this.fillReg(reg => f(get(reg)))
    }

    /*implicit final class RegisterMaker[A](x: A) {
        def makeReg[B](body: Reg[A] => Parsley[B]): Parsley[B] = pure(x).fillReg(body)
    }*/

    /** `forP(init, cond, step)(body)` behaves much like a traditional for loop using `init`, `cond`, `step` and `body` as parsers
      * which control the loop itself. This is useful for performing certain context sensitive tasks. For instance, to read an equal number of as,
      * bs and cs you can do:
      *
      * {{{
      * val r = Reg.make[Int]
      *
      * r.put(0) *>
      * many('a' *> r.modify(_+1)) *>
      * forP[Int](r.get, pure(_ != 0), pure(_ - 1)){_ => 'b'} *>
      * forP[Int](r.get, pure(_ != 0), pure(_ - 1)){_ => 'c'}
      * }}}
      *
      * @param init The initial value of the induction variable
      * @param cond The condition by which the loop terminates
      * @param step The change in induction variable on each iteration
      * @param body The body of the loop performed each iteration
      * @group comb
      */
    private def forP_[A](init: Parsley[A], cond: =>Parsley[A => Boolean], step: =>Parsley[A => A])(body: Parsley[A] => Parsley[_]): Parsley[Unit] = {
        /*val reg = Reg.make[A]
        val _cond = reg.gets(cond)
        val _step = reg.modify(step)
        reg.put(init) *> when(_cond, whileP(body(reg) *> _step *> _cond))*/
        import parsley.Parsley.unit
        lazy val _cond = cond
        lazy val _step = step
        def loop(x: A): Parsley[Unit] =
            _cond.flatMap { p =>
                if (!p(x)) unit else {
                    body(pure(x)) *>
                    _step.flatMap { f =>
                        loop(f(x))
                    }
                }
            }
        init.flatMap(loop)
    }

    /** `forP(init, cond, step)(body)` behaves much like a traditional for loop using `init`, `cond`, `step` and `body` as parsers
      * which control the loop itself. This is useful for performing certain context sensitive tasks. For instance, to read an equal number of as,
      * bs and cs you can do:
      *
      * {{{
      * val r = Reg.make[Int]
      *
      * r.put(0) *>
      * many('a' *> r.modify(_+1)) *>
      * forP[Int](r.get, pure(_ != 0), pure(_ - 1)){'b'} *>
      * forP[Int](r.get, pure(_ != 0), pure(_ - 1)){'c'}
      * }}}
      *
      * @param init The initial value of the induction variable
      * @param cond The condition by which the loop terminates
      * @param step The change in induction variable on each iteration
      * @param body The body of the loop performed each iteration
      * @group comb
      */
    def forP[A](init: Parsley[A], cond: =>Parsley[A => Boolean], step: =>Parsley[A => A])(body: =>Parsley[_]): Parsley[Unit] = {
        lazy val _body = body
        forP_(init, cond, step) { _ =>
            _body
        }
    }
}
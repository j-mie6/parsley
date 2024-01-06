/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley

import scala.collection.mutable

import parsley.combinator.{whenS, whileS}
import parsley.syntax.zipped.Zipped2

import parsley.internal.deepembedding.frontend

import Parsley.{fresh, pure}
import org.typelevel.scalaccompat.annotation.nowarn

/** This module contains all the functionality and operations for using and manipulating references.
  *
  * These often have a role in performing context-sensitive parsing tasks, where a Turing-powerful
  * system is required. While `flatMap` is capable of such parsing, it is much less efficient
  * than the use of references, though slightly more flexible. In particular, the `persist` combinator
  * enabled by `StateCombinators` can serve as a drop-in replacement for `flatMap` in many scenarios.
  *
  * @since 4.5.0
  *
  * @groupprio ref 0
  * @groupname ref References
  * @groupdesc ref
  *     The `Ref` type is used to describe pieces of state that are threaded through a parser.
  *     The creation and basic combinators of references are found within `Ref` and its companion
  *     object.
  *
  * @groupprio comb 5
  * @groupname comb Reference-Based Combinators
  * @groupdesc comb
  *     Some combinators are made much more efficient in the presence of references and they can
  *     be found here.
  *
  * @groupprio ext 10
  * @groupname ext Reference Extension Combinators
  * @groupdesc ext
  *     These are implicit classes that, when in scope, enable additional combinators on
  *     parsers that interact with the reference system in some way.
  */
object state {
    /** This class is used to index references within the mutable state.
      *
      * @note it is undefined behaviour to use a reference in multiple different
      *       independent parsers. You should be careful to parameterise the
      *       references in shared parsers and allocate fresh ones for each "top-level"
      *       parser you will run.
      * @group ref
      *
      * @groupprio getters 0
      * @groupname getters Getters
      * @groupdesc getters
      *   These combinators allow for the retrieval of the stateful value of a reference, and
      *   injecting it into the parsing context. Does not modify the contents of the reference
      *   itself.
      *
      * @groupprio setters 5
      * @groupname setters Setters
      * @groupdesc setters
      *   These combinators directly update the value contained within a reference. This new
      *   value can be provided directly or sourced from a parser.
      *
      * @groupprio mod 10
      * @groupname mod Modification
      * @groupdesc mod
      *   These combinators modify the value stored within a reference by using a function.
      *   The function used can be provided directly or sourced from a parser.
      *
      * @groupprio local 15
      * @groupname local Local Modification
      * @groupdesc local
      *   These combinators allow for some form of local stateful modification. This means
      *   that any changes to the reference may be reverted after the execution of the parser:
      *   this may be on the parsers success, but it could also involve the parsers failure.
      */
    type Ref[A] = parsley.registers.Reg[A] @nowarn

    /** This object allows for the construction of a reference via its `make` function.
      * @group ref
      */
    object Ref {
        /** This function creates a new (global) reference of a given type.
          *
          * The reference created by this function is not allocated to any specific parser until it has been
          * used by a parser. It should not be used with multiple different parsers.
          *
          * @tparam A the type to be contained in this reference during runtime
          * @return a new reference which can contain the given type.
          * @note references created in this manner ''must'' be initialised in the top-level parser and not
          *       inside a `flatMap`, as this may make them corrupt other references. They should be used with
          *       caution. It is recommended to use `makeRef` and `fillRef` where possible.
          * @since 2.2.0
          */
        def make[A]: Ref[A] = new parsley.registers.Reg: @nowarn
    }

    /** This class, when in scope, enables the use of combinators directly on parsers
      * that interact with the reference system to store and persist results so they
      * can be used multiple times.
      *
      * @constructor This constructor should not be called manually, it is designed to be used via Scala's implicit resolution.
      * @param p the value that this class is enabling methods on.
      * @param con a conversion that allows values convertible to parsers to be used.
      * @group ext
      */
    implicit final class StateCombinators[P, A](p: P)(implicit con: P => Parsley[A]) {
        /** This combinator fills a fresh reference with the result of this parser, this
          * reference is provided to the given function, which continues the parse.
          *
          * This allows for a more controlled way of creating references during a parse,
          * without explicitly creating them with `Ref.make[A]` and using `put`. These
          * references are intended to be fresh every time they are "created", in other
          * words, a recursive call with a `fillReg` call inside will modify a different
          * reference each time.
          *
          * @example {{{
          * // this is an efficient implementation for persist.
          * def persist[B](f: Parsley[A] => Parsley[B]): Parsley[B] = this.fillRef(reg => f(ref.get))
          * }}}
          *
          * @param body a function to generate a parser that can interact with the freshly created reference.
          * @since 4.0.0
          */
        def fillRef[B](body: Ref[A] => Parsley[B]): Parsley[B] = {
            val ref = Ref.make[A]
            new Parsley(new frontend.NewReg(ref, con(p).internal, body(ref).internal))
        }
        /** This combinator allows for the result of this parser to be used multiple times within a function,
          * without needing to reparse or recompute.
          *
          * Similar to `flatMap`, except it is much cheaper to do, at the cost of the restriction that the argument is `Parsley[A]` and not just `A`.
          *
          * @example {{{
          * // this is a reasonable implementation, though direct use of `branch` may be more efficent.
          * def filter(pred: A => Boolean): Parsley[A] = {
          *     this.persist(px => ifS(px.map(pred), px, empty))
          * }
          * }}}
          *
          * @param f a function to generate a new parser that can observe the result of this parser many times without reparsing.
          * @since 3.2.0
          */
        def persist[B](f: Parsley[A] => Parsley[B]): Parsley[B] = this.fillRef(ref => f(ref.get))
    }

    /** This class, when in scope, enables a method to create and fill a reference with a
      * given value.
      *
      * @constructor This constructor should not be called manually, it is designed to be used via Scala's implicit resolution.
      * @param x the value to initialise a reference with.
      * @group ext
      */
    implicit final class RefMaker[A](val x: A) extends AnyVal {
        /** This combinator fills a fresh reference with the this value.
          *
          * This allows for a more controlled way of creating references during a parse,
          * without explicitly creating them with `Ref.make[A]` and using `set`. These
          * references are intended to be fresh every time they are "created", in other
          * words, a recursive call with a `makeRef` call inside will modify a different
          * reference.
          *
          * @param body a function to generate a parser that can interact with the freshly created reference.
          * @see [[parsley.state.StateCombinators.fillRef `fillRef`]] for a version that uses the result of a parser to fill the reference instead.
          * @since 4.0.0
          */
        def makeRef[B](body: Ref[A] => Parsley[B]): Parsley[B] = pure(x).fillRef(body)
    }

    /** This combinator allows for the repeated execution of a parser `body` in a stateful loop, `body` will have access to the current value of the state.
      *
      * `forP_(init, cond, step)(body)` behaves much like a traditional for loop using `init`, `cond`, `step` and `body` as parsers
      * which control the loop itself. First, a reference `r` is created and initialised with `init`. Then `cond` is parsed, producing
      * the function `pred`. If `r.gets(pred)` returns true, then `body` is parsed, then `r` is modified with the result of parsing `step`.
      * This repeats until `r.gets(pred)` returns false. This is useful for performing certain context sensitive tasks.
      *
      * @example the classic context sensitive grammar of `a^n^b^n^c^n^` can be matched using `forP_`:
      * {{{
      * val r = Ref.make[Int]
      *
      * r.set(0) *>
      * many('a' *> r.update(_+1)) *>
      * forP_[Int](r.get, pure(_ != 0), pure(_ - 1)){_ => 'b'} *>
      * forP_[Int](r.get, pure(_ != 0), pure(_ - 1)){_ => 'c'}
      * }}}
      *
      * @param init the initial value of the induction variable.
      * @param cond the condition by which the loop terminates.
      * @param step the change in induction variable on each iteration.
      * @param body the body of the loop performed each iteration, which has access to the current value of the state.
      * @return a parser that initialises some state with `init` and then parses body until `cond` is true, modifying the state each iteration with `step`.
      * @see [[parsley.state.forYieldP_ `forYieldP_`]] for a version that returns the results of each `body` parse.
      * @group comb
      */
    def forP_[A](init: Parsley[A], cond: =>Parsley[A => Boolean], step: =>Parsley[A => A])(body: Parsley[A] => Parsley[_]): Parsley[Unit] = {
        init.fillRef { ref =>
          lazy val _cond = ref.gets(cond)
          lazy val _step = ref.update(step)
          whenS(_cond, whileS(body(ref.get) *> _step *> _cond))
        }
    }

    /** This combinator allows for the repeated execution of a parser `body` in a stateful loop, `body` will have access to the current value of the state.
      *
      * `forP_(init, cond, step)(body)` behaves much like a traditional for comprehension using `init`, `cond`, `step` and `body` as parsers
      * which control the loop itself. First, a reference `r` is created and initialised with `init`. Then `cond` is parsed, producing
      * the function `pred`. If `r.gets(pred)` returns true, then `body` is parsed, then `r` is modified with the result of parsing `step`.
      * This repeats until `r.gets(pred)` returns false. This is useful for performing certain context sensitive tasks. Unlike `forP_` the
      * results of the body invokations are returned in a list.
      *
      * @example the classic context sensitive grammar of `a^n^b^n^c^n^` can be matched using `forP_`:
      * {{{
      * val r = Ref.make[Int]
      *
      * r.set(0) *>
      * many('a' *> r.update(_+1)) *>
      * forYieldP_[Int, Char](r.get, pure(_ != 0), pure(_ - 1)){_ => 'b'} *>
      * forYieldP_[Int, Char](r.get, pure(_ != 0), pure(_ - 1)){_ => 'c'}
      * }}}
      *
      * This will return a list `n` `'c'` characters.
      *
      * @param init the initial value of the induction variable.
      * @param cond the condition by which the loop terminates.
      * @param step the change in induction variable on each iteration.
      * @param body the body of the loop performed each iteration, which has access to the current value of the state.
      * @return a parser that initialises some state with `init` and then parses body until `cond` is true, modifying the state each iteration with `step`.
      * @see [[parsley.state.forP_ `forP_`]] for a version that ignores the results of the body.
      * @group comb
      */
    def forYieldP_[A, B](init: Parsley[A], cond: =>Parsley[A => Boolean], step: =>Parsley[A => A])(body: Parsley[A] => Parsley[B]): Parsley[List[B]] = {
        fresh(mutable.ListBuffer.empty[B]).persist { acc =>
            forP_(init, cond, step) { x =>
                (acc, body(x)).zipped(_ += _).impure // we don't want this optimised out, it's a mutable operation in a resultless context
            } ~> acc.map(_.toList)
        }
    }

    /** This combinator allows for the repeated execution of a parser in a stateful loop.
      *
      * `forP(init, cond, step)(body)` behaves much like a traditional for loop using `init`, `cond`, `step` and `body` as parsers
      * which control the loop itself. First, a reference `r` is created and initialised with `init`. Then `cond` is parsed, producing
      * the function `pred`. If `r.gets(pred)` returns true, then `body` is parsed, then `r` is modified with the result of parsing `step`.
      * This repeats until `r.gets(pred)` returns false. This is useful for performing certain context sensitive tasks.
      *
      * @example the classic context sensitive grammar of `a^n^b^n^c^n^` can be matched using `forP`:
      * {{{
      * val r = Ref.make[Int]
      *
      * r.set(0) *>
      * many('a' *> r.update(_+1)) *>
      * forP[Int](r.get, pure(_ != 0), pure(_ - 1)){'b'} *>
      * forP[Int](r.get, pure(_ != 0), pure(_ - 1)){'c'}
      * }}}
      *
      * @param init the initial value of the induction variable.
      * @param cond the condition by which the loop terminates.
      * @param step the change in induction variable on each iteration.
      * @param body the body of the loop performed each iteration.
      * @return a parser that initialises some state with `init` and then parses body until `cond` is true, modifying the state each iteration with `step`.
      * @see [[parsley.state.forYieldP `forYieldP`]] for a version that returns the results of each `body` parse.
      * @group comb
      */
    def forP[A](init: Parsley[A], cond: =>Parsley[A => Boolean], step: =>Parsley[A => A])(body: =>Parsley[_]): Parsley[Unit] = {
        lazy val _body = body
        forP_(init, cond, step) { _ =>
            _body
        }
    }

    /** This combinator allows for the repeated execution of a parser in a stateful loop.
      *
      * `forYieldP(init, cond, step)(body)` behaves much like a traditional for comprehension using `init`, `cond`, `step` and `body` as parsers
      * which control the loop itself. First, a reference `r` is created and initialised with `init`. Then `cond` is parsed, producing
      * the function `pred`. If `r.gets(pred)` returns true, then `body` is parsed, then `r` is modified with the result of parsing `step`.
      * This repeats until `r.gets(pred)` returns false. This is useful for performing certain context sensitive tasks. Unlike `forP` the
      * results of the body invokations are returned in a list.
      *
      * @example the classic context sensitive grammar of `a^n^b^n^c^n^` can be matched using `forP`:
      * {{{
      * val r = Ref.make[Int]
      *
      * r.set(0) *>
      * many('a' *> r.update(_+1)) *>
      * forYieldP[Int, Char](r.get, pure(_ != 0), pure(_ - 1)){'b'} *>
      * forYieldP[Int, Char](r.get, pure(_ != 0), pure(_ - 1)){'c'}
      * }}}
      *
      * This will return a list `n` `'c'` characters.
      *
      * @param init the initial value of the induction variable.
      * @param cond the condition by which the loop terminates.
      * @param step the change in induction variable on each iteration.
      * @param body the body of the loop performed each iteration.
      * @return a parser that initialises some state with `init` and then parses body until `cond` is true, modifying the state each iteration with `step`.
      *         The results of the iterations are returned in a list.
      * @see [[parsley.state.forP `forP`]] for a version that ignores the results.
      * @group comb
      */
    def forYieldP[A, B](init: Parsley[A], cond: =>Parsley[A => Boolean], step: =>Parsley[A => A])(body: =>Parsley[B]): Parsley[List[B]] = {
        fresh(mutable.ListBuffer.empty[B]).persist { acc =>
            forP(init, cond, step) {
                (acc, body).zipped(_ += _).impure
            } ~> acc.map(_.toList)
        }
    }
}

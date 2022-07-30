package parsley

/** This module contains `ap1` through `ap22`, which allow for the application of a parser returning a function of arity `N` to `N` parsers.
  *
  * The combinators contained in this module all sequence a number of parsers together, but are capable of combining the
  * results generated by these parsers into a single value with a function of the correct arity produced by the first parser. This is a clean
  * way of putting together multiple parsers and getting a meaningful result out.
  *
  * @example {{{
  * scala> import parsley.character.char
  * scala> import parsley.ap.{ap2, ap3}
  * scala> case class Add(x: Int, y: Int)
  * scala> val p = ap2(pure(Add), char('a') #> 4, char('b') #> 5)
  * scala> p.parse("ab")
  * val res0 = Success(Add(4, 5))
  * scala> val q = ap3(pure((x: Int, y: Int, z: Int) => x * y + z), char('a') #> 3, char('b') #> 2, char('c') #> 5)
  * scala> q.parse("abc")
  * val res1 = Success(11)
  * scala> q.parse("ab")
  * val res2 = Failure(..)
  * }}}
  * @since 4.0.0
  *
  * @define body
  *     This combinator applies the given parsers in sequence and then applies the function returned by `pf` of to all of the results of the other parsers.
  *
  *     Firstly, each parser is parsed in turn, each producing a result (and the first, a function `f`). So long as all of the parsers succeeded,
  *     the combinator can succeed by returning the application of the function `f` to all the arguments. If any
  *     of the parsers fails, the entire combinator fails.
  *
  * @define param a parser that returns a function to apply to the results of the parsers with arity
  * @define return a parser that parses all of the given parsers in order, and then combines their results with `f`.
  */
object ap {
    // scalastyle:off parameter.number ensure.single.space.after.token
    // $COVERAGE-OFF$
    /** This combinator allows the function that results from one parser to be applied to the result of another parser.
      *
      * Effectively alias for `<*>`, to be consistent with the other `ap` variants.
      *
      * @param pf the parser whose result is a function to map across the result of `p1`.
      * @param px the second parser to perform.
      * @return a parser that applies the function `f` resulting from `pf` to the result `x` of the parser `p1`.
      */
    def ap1[T1, R]
        (pf: Parsley[T1 => R],
         p1: =>Parsley[T1]): Parsley[R] =
        pf <*> p1
    /** $body
      *
      * @param pf $param two.
      * @return $return
      */
    // TODO: This can be improved, and lift2 can go via this
    def ap2[T1, T2, R]
        (pf: Parsley[(T1, T2) => R],
         p1: =>Parsley[T1], p2: =>Parsley[T2]): Parsley[R] =
        pf.map(_.curried) <*> p1 <*> p2
    /** $body
      *
      * @param pf $param three.
      * @return $return
      */
    // TODO: This can be improved, and lift3 can go via this
    def ap3[T1, T2, T3, R]
        (pf: Parsley[(T1, T2, T3) => R],
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3]): Parsley[R] =
        pf.map(_.curried) <*> p1 <*> p2 <*> p3
    // $COVERAGE-ON$
    // $COVERAGE-OFF$
    /** $body
      *
      * @param pf $param four.
      * @return $return
      */
    def ap4[T1, T2, T3, T4, R]
        (pf: Parsley[(T1, T2, T3, T4) => R],
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4]): Parsley[R] =
        _ap4(pf.map(_.curried), p1, p2, p3, p4)
    /** $body
      *
      * @param pf $param five.
      * @return $return
      */
    def ap5[T1, T2, T3, T4, T5, R]
        (pf: Parsley[(T1, T2, T3, T4, T5) => R],
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5]): Parsley[R] =
        _ap5(pf.map(_.curried), p1, p2, p3, p4, p5)
    /** $body
      *
      * @param pf $param six.
      * @return $return
      */
    def ap6[T1, T2, T3, T4, T5, T6, R]
        (pf: Parsley[(T1, T2, T3, T4, T5, T6) => R],
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6]): Parsley[R] =
        _ap6(pf.map(_.curried), p1, p2, p3, p4, p5, p6)
    /** $body
      *
      * @param pf $param seven.
      * @return $return
      */
    def ap7[T1, T2, T3, T4, T5, T6, T7, R]
        (pf: Parsley[(T1, T2, T3, T4, T5, T6, T7) => R],
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7]): Parsley[R] =
        _ap7(pf.map(_.curried), p1, p2, p3, p4, p5, p6, p7)
    /** $body
      *
      * @param pf $param eight.
      * @return $return
      */
    def ap8[T1, T2, T3, T4, T5, T6, T7, T8, R]
        (pf: Parsley[(T1, T2, T3, T4, T5, T6, T7, T8) => R],
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7],
         p8: =>Parsley[T8]): Parsley[R] =
        _ap8(pf.map(_.curried), p1, p2, p3, p4, p5, p6, p7, p8)
    /** $body
      *
      * @param pf $param nine.
      * @return $return
      */
    def ap9[T1, T2, T3, T4, T5, T6, T7, T8, T9, R]
        (pf: Parsley[(T1, T2, T3, T4, T5, T6, T7, T8, T9) => R],
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9]): Parsley[R] =
        _ap9(pf.map(_.curried), p1, p2, p3, p4, p5, p6, p7, p8, p9)
    /** $body
      *
      * @param pf $param ten.
      * @return $return
      */
    def ap10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R]
        (pf: Parsley[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => R],
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10]): Parsley[R] =
        _ap10(pf.map(_.curried), p1, p2, p3, p4, p5, p6, p7, p8, p9, p10)
    /** $body
      *
      * @param pf $param eleven.
      * @return $return
      */
    def ap11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R]
        (pf: Parsley[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => R],
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11]): Parsley[R] =
        _ap11(pf.map(_.curried), p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11)
    /** $body
      *
      * @param pf $param twelve.
      * @return $return
      */
    def ap12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R]
        (pf: Parsley[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => R],
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12]): Parsley[R] =
        _ap12(pf.map(_.curried), p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12)
    /** $body
      *
      * @param pf $param thirteen.
      * @return $return
      */
    def ap13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R]
        (pf: Parsley[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => R],
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12], p13: =>Parsley[T13]): Parsley[R] =
        _ap13(pf.map(_.curried), p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13)
    /** $body
      *
      * @param pf $param fourteen.
      * @return $return
      */
    def ap14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R]
        (pf: Parsley[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => R],
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12], p13: =>Parsley[T13], p14: =>Parsley[T14]): Parsley[R] =
        _ap14(pf.map(_.curried), p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14)
    /** $body
      *
      * @param pf $param fifteen.
      * @return $return
      */
    def ap15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R]
        (pf: Parsley[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => R],
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12], p13: =>Parsley[T13], p14: =>Parsley[T14],
         p15: =>Parsley[T15]): Parsley[R] =
        _ap15(pf.map(_.curried), p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15)
    /** $body
      *
      * @param pf $param sixteen.
      * @return $return
      */
    def ap16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R]
        (pf: Parsley[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => R],
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12], p13: =>Parsley[T13], p14: =>Parsley[T14], p15: =>Parsley[T15],
         p16: =>Parsley[T16]): Parsley[R] =
        _ap16(pf.map(_.curried), p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16)
    /** $body
      *
      * @param pf $param seventeen.
      * @return $return
      */
    def ap17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R]
        (pf: Parsley[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => R],
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12], p13: =>Parsley[T13], p14: =>Parsley[T14], p15: =>Parsley[T15],
         p16: =>Parsley[T16], p17: =>Parsley[T17]): Parsley[R] =
        _ap17(pf.map(_.curried), p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17)
    /** $body
      *
      * @param pf $param eighteen.
      * @return $return
      */
    def ap18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R]
        (pf: Parsley[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => R],
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12], p13: =>Parsley[T13], p14: =>Parsley[T14], p15: =>Parsley[T15],
         p16: =>Parsley[T16], p17: =>Parsley[T17], p18: =>Parsley[T18]): Parsley[R] =
        _ap18(pf.map(_.curried), p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18)
    /** $body
      *
      * @param pf $param nineteen.
      * @return $return
      */
    def ap19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R]
        (pf: Parsley[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => R],
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12], p13: =>Parsley[T13], p14: =>Parsley[T14], p15: =>Parsley[T15],
         p16: =>Parsley[T16], p17: =>Parsley[T17], p18: =>Parsley[T18], p19: =>Parsley[T19]): Parsley[R] =
        _ap19(pf.map(_.curried), p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19)
    /** $body
      *
      * @param pf $param twenty.
      * @return $return
      */
    def ap20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R]
        (pf: Parsley[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => R],
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12], p13: =>Parsley[T13], p14: =>Parsley[T14], p15: =>Parsley[T15],
         p16: =>Parsley[T16], p17: =>Parsley[T17], p18: =>Parsley[T18], p19: =>Parsley[T19], p20: =>Parsley[T20]): Parsley[R] =
        _ap20(pf.map(_.curried), p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20)
    /** $body
      *
      * @param pf $param twenty-one.
      * @return $return
      */
    def ap21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R]
        (pf: Parsley[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => R],
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12], p13: =>Parsley[T13], p14: =>Parsley[T14], p15: =>Parsley[T15],
         p16: =>Parsley[T16], p17: =>Parsley[T17], p18: =>Parsley[T18], p19: =>Parsley[T19], p20: =>Parsley[T20], p21: =>Parsley[T21]): Parsley[R] =
        _ap21(pf.map(_.curried), p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21)
    // $COVERAGE-ON$
    /** $body
      *
      * @param pf $param twenty-two.
      * @return $return
      */
    def ap22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, R]
        (pf: Parsley[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) => R],
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12], p13: =>Parsley[T13], p14: =>Parsley[T14], p15: =>Parsley[T15],
         p16: =>Parsley[T16], p17: =>Parsley[T17], p18: =>Parsley[T18], p19: =>Parsley[T19], p20: =>Parsley[T20], p21: =>Parsley[T21],
         p22: =>Parsley[T22]): Parsley[R] =
        _ap22(pf.map(_.curried), p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22)
    // INTERNAL HELPERS
    @inline private def _ap4[T1, T2, T3, T4, R]
        (pf: Parsley[T1 => T2 => T3 => T4 => R],
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4]): Parsley[R] =
        pf <*> p1 <*> p2 <*> p3 <*> p4
    @inline private def _ap5[T1, T2, T3, T4, T5, R]
        (pf: Parsley[T1 => T2 => T3 => T4 => T5 => R],
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5]): Parsley[R] =
        _ap4(pf, p1, p2, p3, p4) <*> p5
    @inline private def _ap6[T1, T2, T3, T4, T5, T6, R]
        (pf: Parsley[T1 => T2 => T3 => T4 => T5 => T6 => R],
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6]): Parsley[R] =
        _ap5(pf, p1, p2, p3, p4, p5) <*> p6
    @inline private def _ap7[T1, T2, T3, T4, T5, T6, T7, R]
        (pf: Parsley[T1 => T2 => T3 => T4 => T5 => T6 => T7 => R],
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7]): Parsley[R] =
        _ap6(pf, p1, p2, p3, p4, p5, p6) <*> p7
    @inline private def _ap8[T1, T2, T3, T4, T5, T6, T7, T8, R]
        (pf: Parsley[T1 => T2 => T3 => T4 => T5 => T6 => T7 => T8 => R],
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7],
         p8: =>Parsley[T8]): Parsley[R] =
        _ap7(pf, p1, p2, p3, p4, p5, p6, p7) <*> p8
    @inline private def _ap9[T1, T2, T3, T4, T5, T6, T7, T8, T9, R]
        (pf: Parsley[T1 => T2 => T3 => T4 => T5 => T6 => T7 => T8 => T9 => R],
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9]): Parsley[R] =
        _ap8(pf, p1, p2, p3, p4, p5, p6, p7, p8) <*> p9
    @inline private def _ap10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R]
        (pf: Parsley[T1 => T2 => T3 => T4 => T5 => T6 => T7 => T8 => T9 => T10 => R],
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10]): Parsley[R] =
        _ap9(pf, p1, p2, p3, p4, p5, p6, p7, p8, p9) <*> p10
    @inline private def _ap11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R]
        (pf: Parsley[T1 => T2 => T3 => T4 => T5 => T6 => T7 => T8 => T9 => T10 => T11 => R],
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11]): Parsley[R] =
        _ap10(pf, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10) <*> p11
    @inline private def _ap12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R]
        (pf: Parsley[T1 => T2 => T3 => T4 => T5 => T6 => T7 => T8 => T9 => T10 => T11 => T12 => R],
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12]): Parsley[R] =
        _ap11(pf, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11) <*> p12
    @inline private def _ap13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R]
        (pf: Parsley[T1 => T2 => T3 => T4 => T5 => T6 => T7 => T8 => T9 => T10 => T11 => T12 => T13 => R],
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12], p13: =>Parsley[T13]): Parsley[R] =
        _ap12(pf, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12) <*> p13
    @inline private def _ap14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R]
        (pf: Parsley[T1 => T2 => T3 => T4 => T5 => T6 => T7 => T8 => T9 => T10 => T11 => T12 => T13 => T14 => R],
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12], p13: =>Parsley[T13], p14: =>Parsley[T14]): Parsley[R] =
        _ap13(pf, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13) <*> p14
    @inline private def _ap15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R]
        (pf: Parsley[T1 => T2 => T3 => T4 => T5 => T6 => T7 => T8 => T9 => T10 => T11 => T12 => T13 => T14 => T15 => R],
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12], p13: =>Parsley[T13], p14: =>Parsley[T14],
         p15: =>Parsley[T15]): Parsley[R] =
        _ap14(pf, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14) <*> p15
    @inline private def _ap16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R]
        (pf: Parsley[T1 => T2 => T3 => T4 => T5 => T6 => T7 => T8 => T9 => T10 => T11 => T12 => T13 => T14 => T15 => T16 => R],
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12], p13: =>Parsley[T13], p14: =>Parsley[T14], p15: =>Parsley[T15],
         p16: =>Parsley[T16]): Parsley[R] =
        _ap15(pf, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15) <*> p16
    @inline private def _ap17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R]
        (pf: Parsley[T1 => T2 => T3 => T4 => T5 => T6 => T7 => T8 => T9 => T10 => T11 => T12 => T13 => T14 => T15 => T16 => T17 => R],
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12], p13: =>Parsley[T13], p14: =>Parsley[T14], p15: =>Parsley[T15],
         p16: =>Parsley[T16], p17: =>Parsley[T17]): Parsley[R] =
        _ap16(pf, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16) <*> p17
    @inline private def _ap18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R]
        (pf: Parsley[T1 => T2 => T3 => T4 => T5 => T6 => T7 => T8 => T9 => T10 => T11 => T12 => T13 => T14 => T15 => T16 => T17 => T18 => R],
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12], p13: =>Parsley[T13], p14: =>Parsley[T14], p15: =>Parsley[T15],
         p16: =>Parsley[T16], p17: =>Parsley[T17], p18: =>Parsley[T18]): Parsley[R] =
        _ap17(pf, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17) <*> p18
    @inline private def _ap19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R]
        (pf: Parsley[T1 => T2 => T3 => T4 => T5 => T6 => T7 => T8 => T9 => T10 => T11 => T12 => T13 => T14 => T15 => T16 => T17 => T18 => T19 => R],
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12], p13: =>Parsley[T13], p14: =>Parsley[T14], p15: =>Parsley[T15],
         p16: =>Parsley[T16], p17: =>Parsley[T17], p18: =>Parsley[T18], p19: =>Parsley[T19]): Parsley[R] =
        _ap18(pf, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18) <*> p19
    @inline private def _ap20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R]
        (pf: Parsley[T1 => T2 => T3 => T4 => T5 => T6 => T7 => T8 => T9 => T10 => T11 => T12 => T13 => T14 => T15 => T16 => T17 => T18 => T19 => T20 => R],
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12], p13: =>Parsley[T13], p14: =>Parsley[T14], p15: =>Parsley[T15],
         p16: =>Parsley[T16], p17: =>Parsley[T17], p18: =>Parsley[T18], p19: =>Parsley[T19], p20: =>Parsley[T20]): Parsley[R] =
        _ap19(pf, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19) <*> p20
    @inline private def _ap21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R]
        (pf: Parsley[T1 => T2 => T3 => T4 => T5 => T6 => T7 => T8 => T9 => T10 => T11 =>
                     T12 => T13 => T14 => T15 => T16 => T17 => T18 => T19 => T20 => T21 => R],
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12], p13: =>Parsley[T13], p14: =>Parsley[T14], p15: =>Parsley[T15],
         p16: =>Parsley[T16], p17: =>Parsley[T17], p18: =>Parsley[T18], p19: =>Parsley[T19], p20: =>Parsley[T20], p21: =>Parsley[T21]): Parsley[R] =
        _ap20(pf, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20) <*> p21
    @inline private def _ap22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, R]
        (pf: Parsley[T1 => T2 => T3 => T4 => T5 => T6 => T7 => T8 => T9 => T10 => T11 =>
                     T12 => T13 => T14 => T15 => T16 => T17 => T18 => T19 => T20 => T21 => T22 => R],
         p1: =>Parsley[T1], p2: =>Parsley[T2], p3: =>Parsley[T3], p4: =>Parsley[T4], p5: =>Parsley[T5], p6: =>Parsley[T6], p7: =>Parsley[T7], p8: =>Parsley[T8],
         p9: =>Parsley[T9], p10: =>Parsley[T10], p11: =>Parsley[T11], p12: =>Parsley[T12], p13: =>Parsley[T13], p14: =>Parsley[T14], p15: =>Parsley[T15],
         p16: =>Parsley[T16], p17: =>Parsley[T17], p18: =>Parsley[T18], p19: =>Parsley[T19], p20: =>Parsley[T20], p21: =>Parsley[T21],
         p22: =>Parsley[T22]): Parsley[R] =
        _ap21(pf, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21) <*> p22
    // scalastyle:on parameter.number ensure.single.space.after.token
}

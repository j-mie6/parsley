/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley

import parsley.errors.ErrorBuilder
import parsley.registers.Reg

import parsley.internal.deepembedding.frontend

import scala.annotation.tailrec
import scala.collection.mutable

/** This module contains the very useful debugging combinator, as well as breakpoints.
  *
  * @groupprio comb 0
  * @groupname comb Debug Combinator Extension Methods
  * @groupdesc comb
  *     These are the debugging combinators, which are enabled by bringing these implicit classes
  *     into scope.
  *
  * @groupprio break 10
  * @groupname break Breakpoints
  * @groupdesc break
  *     These can be used to control how the `debug` combinator operates: when a breakpoint is used
  *     it can halt the execution of the combinator and print out information about the parsers state.
  *
  * @groupprio ctrl 5
  * @groupname ctrl Debug Control
  * @groupdesc ctrl
  *     These methods can control how the debug mechanism functions in a general way.
  */
object debug {
    // $COVERAGE-OFF$
    /** Base trait for breakpoints.
      *
      * @group break
      */
    sealed trait Breakpoint
    /** Indicates that no breaking should occur.
      *
      * @group break
      */
    case object NoBreak extends Breakpoint
    /** Break on entry to the combinator, require user input to advance.
      *
      * @group break
      */
    case object EntryBreak extends Breakpoint
    /** Break on exit to the combinator, require user input to advance.
      *
      * @group break
      */
    case object ExitBreak extends Breakpoint
    /** Break on both entry and exit to the combinator, require user input to advance in both cases.
      *
      * @group break
      */
    case object FullBreak extends Breakpoint

    private [parsley] var renderAscii = false
    /** This method can be used to disable the coloured debug output for terminals that don't support it.
      *
      * @group ctrl
      */
    def disableColourRendering(): Unit = renderAscii = true

    /** This class enables the `debug` combinator on parsers.
      *
      * This extension class operates on values that are convertible to parsers. It enables the use of
      * the `debug` combinator, which can be used to trace the execution through a parser.
      *
      * @constructor This constructor should not be called manually, it is designed to be used via Scala's implicit resolution.
      * @param p the value that this class is enabling methods on.
      * @param con a conversion that allows values convertible to parsers to be used.
      * @tparam P the type of base value that this class is used on (the conversion to `Parsley`) is summoned automatically.
      * @group comb
      *
      * @define debug This combinator allows this parser to be debugged by providing a trace through the execution.
      *
      * When this combinator is entered, it will print the name assigned to the parser,
      * as well as the current currenting input context for a few characters on either side.
      * This parser is then executed. If it succeeded, this combinator again reports the
      * name along with "`Good`" and the input context. If it failed, it reports the name
      * along with "`Bad`" and the input context.
      *
      * When breakpoints are used, the execution of the combinator will pause on either entry,
      * exit, or both. The parse is resumed by entering a newline on standard input. Breakpoints
      * will cause additional information about the internal state of the parser to be reported.
      */
    implicit class DebugCombinators[P, A](p: P)(implicit con: P => Parsley[A]) {
        /** $debug
          *
          * @example {{{
          * scala> import parsley.debug.DebugCombinators, parsley.character.string, parsley.Parsley.attempt
          * scala> val abc = attempt(string("abc").debug("string abc")).debug("attempt")
          * scala> val abd = string("abd").debug("string abd")
          * scala> val p = (abc <|> abd).debug("or")
          * scala> p.parse("abd")
          * >or> (1, 1): abd•
          *              ^
          *   >attempt> (1, 1): abd•
          *                     ^
          *     >string abc> (1, 1): abd•
          *                          ^
          *     <string abc< (1, 3): abd• Fail
          *                            ^
          *   <attempt< (1, 1): abd• Fail
          *                     ^
          *   >string abd> (1, 1): abd•
          *                        ^
          *   <string abd< (1, 4): abd• Good
          *                           ^
          * <or< (1, 4): abd• Good
          *                 ^
          * val res0 = Success("abd")
          * }}}
          *
          * @param name The name to be assigned to this parser
          * @param break The breakpoint properties of this parser, defaults to NoBreak
          * @param coloured Whether to render with colour (default true: render colours)
          * @param watchedRegs Which registers to also track the values of and their names, if any
          */
        def debug(name: String, break: Breakpoint, coloured: Boolean, watchedRegs: (Reg[_], String)*): Parsley[A] = {
            new Parsley(new frontend.Debug[A](con(p).internal, name, !coloured, break, watchedRegs))
        }

        private [parsley] def debug(name: String, break: Breakpoint, coloured: Boolean): Parsley[A] = {
            debug(name, break, coloured, Seq.empty[(Reg[_], String)]: _*)
        }

        /** $debug
          *
          * @example {{{
          * scala> import parsley.debug.DebugCombinators, parsley.character.string, parsley.Parsley.attempt
          * scala> val abc = attempt(string("abc").debug("string abc")).debug("attempt")
          * scala> val abd = string("abd").debug("string abd")
          * scala> val p = (abc <|> abd).debug("or")
          * scala> p.parse("abd")
          * >or> (1, 1): abd•
          *              ^
          *   >attempt> (1, 1): abd•
          *                     ^
          *     >string abc> (1, 1): abd•
          *                          ^
          *     <string abc< (1, 3): abd• Fail
          *                            ^
          *   <attempt< (1, 1): abd• Fail
          *                     ^
          *   >string abd> (1, 1): abd•
          *                        ^
          *   <string abd< (1, 4): abd• Good
          *                           ^
          * <or< (1, 4): abd• Good
          *                 ^
          * val res0 = Success("abd")
          * }}}
          *
          * Renders in colour.
          *
          * @param name The name to be assigned to this parser
          * @param break The breakpoint properties of this parser, defaults to NoBreak
          * @param watchedRegs Which registers to also track the values of and their names, if any
          */
        def debug(name: String, break: Breakpoint, watchedRegs: (Reg[_], String)*): Parsley[A] = debug(name, break, coloured = true, watchedRegs: _*)

        private [parsley] def debug(name: String, break: Breakpoint): Parsley[A] = debug(name, break, Seq.empty[(Reg[_], String)]: _*)

        /** $debug
          *
          * @example {{{
          * scala> import parsley.debug.DebugCombinators, parsley.character.string, parsley.Parsley.attempt
          * scala> val abc = attempt(string("abc").debug("string abc")).debug("attempt")
          * scala> val abd = string("abd").debug("string abd")
          * scala> val p = (abc <|> abd).debug("or")
          * scala> p.parse("abd")
          * >or> (1, 1): abd•
          *              ^
          *   >attempt> (1, 1): abd•
          *                     ^
          *     >string abc> (1, 1): abd•
          *                          ^
          *     <string abc< (1, 3): abd• Fail
          *                            ^
          *   <attempt< (1, 1): abd• Fail
          *                     ^
          *   >string abd> (1, 1): abd•
          *                        ^
          *   <string abd< (1, 4): abd• Good
          *                           ^
          * <or< (1, 4): abd• Good
          *                 ^
          * val res0 = Success("abd")
          * }}}
          *
          * No break-points.
          *
          * @param name The name to be assigned to this parser
          * @param coloured Whether to render with colour
          * @param watchedRegs Which registers to also track the values of and their names, if any
          */
        def debug(name: String, coloured: Boolean, watchedRegs: (Reg[_], String)*): Parsley[A] = debug(name, break = NoBreak, coloured, watchedRegs: _*)

        private [parsley] def debug(name: String, coloured: Boolean): Parsley[A] = debug(name, coloured, Seq.empty[(Reg[_], String)]: _*)

        /** $debug
          *
          * @example {{{
          * scala> import parsley.debug.DebugCombinators, parsley.character.string, parsley.Parsley.attempt
          * scala> val abc = attempt(string("abc").debug("string abc")).debug("attempt")
          * scala> val abd = string("abd").debug("string abd")
          * scala> val p = (abc <|> abd).debug("or")
          * scala> p.parse("abd")
          * >or> (1, 1): abd•
          *              ^
          *   >attempt> (1, 1): abd•
          *                     ^
          *     >string abc> (1, 1): abd•
          *                          ^
          *     <string abc< (1, 3): abd• Fail
          *                            ^
          *   <attempt< (1, 1): abd• Fail
          *                     ^
          *   >string abd> (1, 1): abd•
          *                        ^
          *   <string abd< (1, 4): abd• Good
          *                           ^
          * <or< (1, 4): abd• Good
          *                 ^
          * val res0 = Success("abd")
          * }}}
          *
          * Renders in colour with no break-point.
          *
          * @param name The name to be assigned to this parser
          * @param watchedRegs Which registers to also track the values of and their names, if any
          */
        def debug(name: String, watchedRegs: (Reg[_], String)*): Parsley[A] = debug(name, break = NoBreak, coloured = true, watchedRegs: _*)

        private [parsley] def debug(name: String): Parsley[A] = debug(name, Seq.empty[(Reg[_], String)]: _*)

        /** Display information about the error messages generated by this parser.
          *
          * This is an experimental debugger that provides internal information about error messages.
          * This provides more detail than one might normally see inside a regular error message, but
          * may help isolate the root cause of an error message not being as expected: this can form the
          * bulk of a specific question on the discussion board.
          *
          * @param name The name to be assigned to this parser
          * @param coloured Whether the output should be colourful
          * @param errBuilder The error builder used for formatting messages in the "real parser",
          *                   which is used to help format information in the debugger.
          * @since 4.0.0
          */
        def debugError(name: String, coloured: Boolean)(implicit errBuilder: ErrorBuilder[_]): Parsley[A] = {
            new Parsley(new frontend.DebugError[A](con(p).internal, name, !coloured, errBuilder))
        }

        /** Display information about the error messages generated by this parser.
          *
          * This is an experimental debugger that provides internal information about error messages.
          * This provides more detail than one might normally see inside a regular error message, but
          * may help isolate the root cause of an error message not being as expected: this can form the
          * bulk of a specific question on the discussion board.
          *
          * @param name The name to be assigned to this parser
          * @param coloured Whether the output should be colourful
          * @param errBuilder The error builder used for formatting messages in the "real parser",
          *                   which is used to help format information in the debugger.
          * @since 4.0.0
          */
        def debugError(name: String)(implicit errBuilder: ErrorBuilder[_]): Parsley[A] = debugError(name, coloured = true)

        /** This combinator allows for the runtime of this parser to be measured.
          *
          * When this parser executes, its start and end times will be logged using `System.nanoTime()`,
          * which has a resolution of 100ns. These will be logged into the given `Profiler` object.
          *
          * @param name the ''unique'' name of this parser, which will represent it in the table
          * @param profiler the profiling object that will collect and process the data
          * @note usual disclaimers about profiling apply: results are just data; use your judgement
          * @see [[Profiler `Profiler`]]
          * @since 4.4.0
          */
        def profile(name: String)(implicit profiler: Profiler): Parsley[A] = new Parsley(new frontend.Profile[A](con(p).internal, name, profiler))
    }
    // $COVERAGE-ON$

    /** This class is used to store the profile data for a specific group of sub-parsers.
      *
      * It records the start and end timestamps of the parsers that interact with it. It is possible
      * to use multiple different profilers if you want to establish the cumulative time for a sub-parser
      * instead of the self-time.
      *
      * This class is mutable, so care must be taken to call `reset()` between runs, unless you want to
      * accumulate the data.
      *
      * @since 4.4.0
      */
    class Profiler {
        private val entries = mutable.Map.empty[String, mutable.Buffer[Long]]
        private val exits = mutable.Map.empty[String, mutable.Buffer[Long]]
        private var lastTime: Long = 0
        private var lastTimeCount: Long = 0

        // $COVERAGE-OFF$
        /** Prints a summary of the data sampled by this profiler.
          *
          * After the run(s) of the parser are complete, this method can be used to
          * generate the summary of the sampled data. It will print a table where the
          * total "self-time", number of invocations and average "self-time" are displayed
          * for each profiled sub-parser.
          *
          * * '''self-time''': this is the amount of time spend in a specific parser, removing
          *                    the times from within the child parsers.
          *
          * @note to measure cumulative time of a parser, consider using a separate `Profiler`
          *       object for it instead.
          * @since 4.4.0
          */
        def summary(): Unit = {
            val (selfTotals, invocations) = process
            render(selfTotals, invocations)
        }
        // $COVERAGE-ON$

        /** Clears the data within this profiler.
          * @since 4.4.0
          */
        def reset(): Unit = {
            // can't clear the maps, because the instructions may have already captured the lists
            for ((_, timings) <- entries) timings.clear()
            for ((_, timings) <- exits) timings.clear()
            lastTime = 0
            lastTimeCount = 0
        }

        private [parsley] def entriesFor(name: String): mutable.Buffer[Long] = entries.getOrElseUpdate(name, mutable.ListBuffer.empty)
        private [parsley] def exitsFor(name: String): mutable.Buffer[Long] = exits.getOrElseUpdate(name, mutable.ListBuffer.empty)
        private [parsley] def monotone(n: Long) = {
            if (n == lastTime) {
                lastTimeCount += 1
                n + lastTimeCount
            }
            else {
                lastTime = n
                lastTimeCount = 0
                n
            }
        }

        private [parsley] def process: (Map[String, Long], Map[String, Int]) = {
            val allEntries = collapse(entries).sortBy(_._2)
            val allExits = collapse(exits).sortBy(_._2)

            require((allEntries ::: allExits).toSet.size == (allExits.length + allExits.length),
                    "recorded times must all be monotonically increasing")

            val selfTotals = mutable.Map.empty[String, Long]
            val invocations =  mutable.Map.empty[String, Int]

            @tailrec
            def go(entries: List[(String, Long)], exits: List[(String, Long)], stack: List[((String, Long), Long)], cum: Long): Unit = {
                (entries, exits, stack) match {
                    case (Nil, Nil, Nil) =>
                    // final unwinding or stuff to clear on the stack (cum here is for the children)
                    case (ens, (n2, t2)::exs, ((n1, t1), oldCum)::stack) if ens.headOption.forall(t2 < _._2) =>
                        assert(n1 == n2, "unwinding should result in matching values")
                        add(invocations, n1)(1)
                        add(selfTotals, n1)(t2 - t1 - cum)
                        go(ens, exs, stack, oldCum + t2 - t1)
                    // in this case, the scope closes quickly (cum here is for your siblings)
                    case ((n1, t1)::ens, (n2, t2)::exs, stack) if ens.headOption.forall(t2 < _._2) && n1 == n2 =>
                        assert(ens.nonEmpty || n1 == n2, "unwinding should result in matching values")
                        add(invocations, n1)(1)
                        add(selfTotals, n1)(t2 - t1)
                        go(ens, exs, stack, cum + t2 - t1)
                    // the next one opens first, or the entry and exit don't match
                    // in either case, this isn't our exit, push ourselves onto the stack (cum here is for your siblings)
                    case (nt::ens, exs@(_ :: _), stack) => go(ens, exs, (nt, cum)::stack, 0)
                    // $COVERAGE-OFF$
                    case (Nil, Nil, _::_)
                       | (Nil, _::_, Nil)
                       | (_ ::_, Nil, _) => assert(false, "something has gone very wrong")
                    case (Nil, _::_, _::_) => ??? // deadcode from case 2
                    // $COVERAGE-ON$
                }
            }

            //println(allEntries.map { case (name, t) => (name, t - allEntries.head._2) })
            //println(allExits.map { case (name, t) => (name, t - allEntries.head._2) })
            go(allEntries, allExits, Nil, 0)
            (selfTotals.toMap, invocations.toMap)
        }

        private def collapse(timings: Iterable[(String, Iterable[Long])]): List[(String, Long)] = timings.flatMap {
            case (name, times) => times.map(t => (name, t))
        }.toList

        private def add[A: Numeric](m: mutable.Map[String, A], name: String)(n: A): Unit = m.get(name) match {
            case Some(x) => m(name) = implicitly[Numeric[A]].plus(x, n)
            case None => m(name) = n
        }

        // $COVERAGE-OFF$
        private def render(selfTimes: Map[String, Long], invocations: Map[String, Int]): Unit = {
            val combined = selfTimes.map {
                case (name, selfTime) =>
                    val invokes = invocations(name)
                    (name, (f"${selfTime/1000.0}%.1fμs", invocations(name), f"${selfTime/invokes/1000.0}%.3fμs"))
            }
            val head1 = "name"
            val head2 = "self time"
            val head3 = "num calls"
            val head4 = "average self time"

            val (names, data) = combined.unzip
            val (selfs, invokes, avs) = data.unzip3

            val col1Width = (head1.length :: names.map(_.length).toList).max
            val col2Width = (head2.length :: selfs.map(_.length).toList).max
            val col3Width = (head3.length :: invokes.map(digits(_)).toList).max
            val col4Width = (head4.length :: avs.map(_.length).toList).max

            val header = List(pad(head1, col1Width), tab(col1Width),
                              pad(head2, col2Width), tab(col2Width),
                              pad(head3, col3Width), tab(col3Width),
                              pad(head4, col4Width)).mkString
            val hline = header.map(_ => '-')

            println(header)
            println(hline)
            for ((name, (selfTime, invokes, avSelfTime)) <- combined) {
                println(List(pad(name, col1Width), tab(col1Width),
                             prePad(selfTime, col2Width), tab(col2Width),
                             prePad(invokes.toString, col3Width), tab(col3Width),
                             prePad(avSelfTime, col4Width)).mkString)
            }
            println(hline)
        }

        private def pad(str: String, n: Int) = str + " " * (n - str.length)
        private def prePad(str: String, n: Int) = " " * (n - str.length) + str
        private def digits[A: Numeric](n: A): Int = Math.log10(implicitly[Numeric[A]].toDouble(n)).toInt + 1
        private def tab(n: Int) = " " * (4 - n % 4)
        // $COVERAGE-ON$
    }
}

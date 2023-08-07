/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.errors

import parsley.Parsley, Parsley.atomic
import parsley.implicits.zipped.Zipped3
import parsley.errors.combinator.{ErrorMethods, amend}
import parsley.position.offset

import parsley.internal.deepembedding.singletons

/** This module contains combinators that help facilitate the error message generational patterns ''Verified Errors'' and ''Preventative Errors''.
  *
  * In particular, exposes an extension class `VerifiedErrors` that facilitates creating verified errors in many different formats.
  *
  * @group combinators
  * @since 4.2.0
  */
object patterns {
    /** This class exposes combinators related to the ''Verified Errors'' parser design pattern.
      *
      * This extension class operates on values that are convertible to parsers. The combinators it enables
      * allow for the parsing of known illegal values, providing richer error messages in case they succeed.
      *
      * @constructor This constructor should not be called manually, it is designed to be used via Scala's implicit resolution.
      * @param p the value that this class is enabling methods on.
      * @param con a conversion that allows values convertible to parsers to be used.
      * @tparam P the type of base value that this class is used on (the conversion to `Parsley`) is summoned automatically.
      * @since 4.2.0
      *
      * @define autoAmend
      *     when this combinator fails (and not this parser itself), it will generate errors rooted at the start of the
      *     parse (as if [[parsley.errors.combinator$.amend `amend`]] had been used) and the caret will span the entire
      *     successful parse of this parser.
      *
      * @define attemptNonTerminal
      *     when this parser is not to be considered as a terminal error, use `attempt` around the ''entire'' combinator to
      *     allow for backtracking if this parser succeeds (and therefore fails).
      *
      * @define Ensures this parser does not succeed, failing with a
      */
    implicit final class VerifiedErrors[P, A](p: P)(implicit con: P => Parsley[A]) {
        /** Ensures this parser does not succeed, failing with a specialised error based on this parsers result if it does.
          *
          * If this parser succeeds, input is consumed and this combinator will fail, producing an error message
          * based on the parsed result. However, if this parser fails, no input is consumed and an empty error is generated.
          * This parser will produce no labels if it fails.
          *
          * @param msggen the function that generates the error messages from the parsed value.
          * @since 4.2.0
          * @note $autoAmend
          * @note $attemptNonTerminal
          */
        def verifiedFail(msggen: A => Seq[String]): Parsley[Nothing] = this.verifiedWith(new Parsley(new singletons.SpecialisedGen(msggen)))

        /** Ensures this parser does not succeed, failing with a specialised error if it does.
          *
          * If this parser succeeds, input is consumed and this combinator will fail, producing an error message
          * based on the given messages. However, if this parser fails, no input is consumed and an empty error is generated.
          * This parser will produce no labels if it fails.
          *
          * @param msg0 the first message in the error message.
          * @param msgs the remaining messages that will make up the error message.
          * @since 4.2.0
          * @note $autoAmend
          * @note $attemptNonTerminal
          */
        def verifiedFail(msg0: String, msgs: String*): Parsley[Nothing] = this.verifiedFail(_ => msg0 +: msgs)

        /** Ensures this parser does not succeed, failing with a vanilla error with an unexpected message and caret spanning the parse.
          *
          * If this parser succeeds, input is consumed and this combinator will fail, producing an unexpected message the same width as
          * the parse. However, if this parser fails, no input is consumed and an empty error is generated.
          * This parser will produce no labels if it fails.
          *
          * @since 4.2.0
          * @note $autoAmend
          * @note $attemptNonTerminal
          */
        def verifiedUnexpected: Parsley[Nothing] = this.verifiedWithVanillaRaw(_ => None)

        /** Ensures this parser does not succeed, failing with a vanilla error with an unexpected message and caret spanning the parse and a given reason.
          *
          * If this parser succeeds, input is consumed and this combinator will fail, producing an unexpected message the same width as
          * the parse along with the given reason. However, if this parser fails, no input is consumed and an empty error is generated.
          * This parser will produce no labels if it fails.
          *
          * @param reason the reason that this parser is illegal.
          * @since 4.2.0
          * @note $autoAmend
          * @note $attemptNonTerminal
          */
        def verifiedUnexpected(reason: String): Parsley[Nothing] = this.verifiedWithVanillaRaw(_ => Some(reason))

        /** Ensures this parser does not succeed, failing with a vanilla error with an unexpected message and caret spanning the parse and a reason generated
          * from this parser's result.
          *
          * If this parser succeeds, input is consumed and this combinator will fail, producing an unexpected message the same width as
          * the parse along with a reason generated from the successful parse. However, if this parser fails, no input is consumed and an empty error
          * is generated. This parser will produce no labels if it fails.
          *
          * @param reason a function that produces a reason for the error given the parsed result.
          * @since 4.2.0
          * @note $autoAmend
          * @note $attemptNonTerminal
          */
        def verifiedUnexpected(reason: A => String): Parsley[Nothing] = this.verifiedWithVanillaRaw(x => Some(reason(x)))


        private def verifiedWith(err: Parsley[((A, Int)) => Nothing]) = amend {
            (offset, atomic(con(p)).newHide, offset).zipped {
                (s, x, e) => (x, e-s)
            } <**> err
        }

        @inline private def verifiedWithVanilla(unexGen: A => UnexpectedItem, reasonGen: A => Option[String]) = {
            verifiedWith(new Parsley(new singletons.VanillaGen(unexGen, reasonGen)))
        }

        @inline private def verifiedWithVanillaRaw(reasonGen: A => Option[String]) = verifiedWithVanilla(_ => UnexpectedItem.Raw, reasonGen)
    }
}

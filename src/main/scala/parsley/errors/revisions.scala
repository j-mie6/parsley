package parsley.errors

/** Revisions help to ensure backwards-compatibility when the `ErrorBuilder` API changes.
  * By mixing in a `Revision` to your instances, you are advertising that you want to remain
  * compatible with that version of the API. If the API changes with ''minor'' version
  * increases, then the revision mixed in will implement a compatiblity layer to ensure
  * that your code should still compile. If you want to opt into a more recent revision,
  * you just change the mixin. A ''major'' version bump will clear the revisions back to
  * `Revision0` again.
  * @since 3.0.0
  */
// $COVERAGE-OFF$
object revisions {
    /** @since 3.0.0 */
    trait Revision0 extends Revision1 { this: ErrorBuilder[_] =>
        /**
          * Describes how to format the information about the line that
          * the error occured on.
          *
          * @param line The full line of input that produced this error
          *             message
          * @param errorPointsAt The offset into the line that the error
          *                      points at
          * @since 3.0.0
          */
        def lineInfo(line: String, errorPointsAt: Int): LineInfo

        final override val numLinesBefore = 0
        final override val numLinesAfter = 0
        final override def lineInfo(line: String, linesBefore: List[String], linesAfter: List[String], errorPointsAt: Int): LineInfo = {
            lineInfo(line, errorPointsAt)
        }
    }
    /** @since 3.1.0 */
    trait Revision1/* extends Revision2*/ { this: ErrorBuilder[_] =>
        //type Context = Unit
        //final override def contexualScope(context: String): Context = ()

        //type NestedContexts = Unit
        //final override def nestContexts(contexts: List[Context]): NestedContexts = ()

        // Make sure this starts life out deprecated
        //def format(pos: Position, source: Source, lines: ErrorInfoLines): _Err
        //final override def format(pos: Position, source: Source, ctxs: NestedContexts, lines: ErrorInfoLines): _Err = format(pos, source, lines)
    }
    /* @since ??? */
    //trait Revision2 { this: ErrorBuilder[_] => }
}
// $COVERAGE-ON$
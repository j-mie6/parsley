package parsley.internal.deepembedding.frontend.debugger

import scala.collection.mutable

import parsley.debugger.internal.DebugContext

import parsley.internal.deepembedding.backend.StrictParsley
import parsley.internal.deepembedding.frontend
import parsley.internal.deepembedding.frontend.LazyParsley

object helpers {
  private [parsley] def traverseDown[A]
  (parser: LazyParsley[A])
  (implicit seen: mutable.Map[LazyParsley[_], Debugged[_]], dbgCtx: DebugContext): LazyParsley[A] =
  // This stops recursive parsers from causing an infinite recursion.
    if (seen.contains(parser)) {
      // Return a parser with a debugger attached.
      seen(parser).asInstanceOf[Debugged[A]]
    } else {
      val current = new Debugged[A](parser, None)

      // Without this, we could potentially have infinite recursion from lazy-initialised parsers.
      seen.put(parser, current)

      // Function is buried in the frontend package to facilitate access to the GeneralisedEmbedding
      // abstract classes and their getters.
      implicit val attached: List[LazyParsley[Any]] = getChildren(parser).map(traverseDown(_))

      // Populate our parser with the new debugged children.
      current.par = Some(reconstruct(parser))

      // Return a parser with a debugger attached.
      seen(parser).asInstanceOf[Debugged[A]]
    }

  // Attempt to retrieve the child parsers.
  private [this] def getChildren(parser: LazyParsley[_]): List[LazyParsley[_]] =
    parser match {
      case p: frontend.Unary[_, _] => List(p.parser)
      case p: frontend.Binary[_, _, _] => List(p.leftParser, p.rightParser)
      case p: frontend.Ternary[_, _, _, _] => List(p.firstParser, p.secondParser, p.thirdParser)
      case p: frontend.<|>[_] => List(p.leftParser, p.rightParser)
      case p: frontend.ChainPre[_] => List(p.itemParser, p.opParser)
      case _ =>
        // This catches all atomic parsers (e.g. satisfy parsers).
        Nil
    }

  // WARNING: very unsafe due to asInstanceOf call.
  private [this] def coerce[A](ix: Int)(implicit children: List[LazyParsley[Any]]): LazyParsley[A] =
    children(ix).asInstanceOf[LazyParsley[A]]

  // Reconstruct the original parser with new components.
  // WARNING: very unsafe due to call to coerce.
  private [this] def reconstruct[A, X, Y, Z]
  (parser: LazyParsley[A])
  (implicit children: List[LazyParsley[Any]]): LazyParsley[A] =
    parser match {
      case par: frontend.Unary[X, A] =>
        new frontend.Unary[X, A](coerce[X](0)) {
          override def make(p: StrictParsley[X]): StrictParsley[A] = par.make(p)
        }
      case par: frontend.Binary[X, Y, A] =>
        new frontend.Binary[X, Y, A](coerce[X](0), coerce[Y](1)) {
          override def make(p: StrictParsley[X], q: StrictParsley[Y]): StrictParsley[A] = par.make(p, q)
        }
      case par: frontend.Ternary[X, Y, Z, A] =>
        new frontend.Ternary[X, Y, Z, A](coerce[X](0), coerce[Y](1), coerce[Z](2)) {
          override def make(p: StrictParsley[X], q: StrictParsley[Y], r: StrictParsley[Z]): StrictParsley[A] =
            par.make(p, q, r)
        }
      case _: frontend.<|>[A] =>
        new frontend.<|>[A](coerce[A](0), coerce[A](1))
      case _: frontend.ChainPre[A] =>
        new frontend.ChainPre[A](coerce[A](0), coerce[A => A](1))
      case _ => parser
    }
}

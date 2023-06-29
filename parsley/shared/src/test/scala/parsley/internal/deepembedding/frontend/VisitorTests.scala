package parsley.internal.deepembedding.frontend

import org.scalatest.Assertion
import parsley.{Parsley, ParsleyTest}
import parsley.debug.FullBreak
import parsley.errors.{DefaultErrorBuilder, ErrorBuilder, Token}
import parsley.internal.collection.immutable.Trie
import parsley.internal.deepembedding.ContOps
import parsley.internal.deepembedding.Sign
import parsley.internal.deepembedding.backend.StrictParsley
import parsley.internal.deepembedding.singletons.*
import parsley.internal.deepembedding.singletons.token.*
import parsley.internal.errors.{CaretWidth, ExpectDesc, ExpectItem, FlexibleCaret}
import parsley.internal.machine.errors.DefuncError
import parsley.registers.Reg
import parsley.token.descriptions.SpaceDesc
import parsley.token.descriptions.numeric.PlusSignPresence
import parsley.token.errors.{ErrorConfig, FilterConfig, LabelConfig, LabelWithExplainConfig, SpecialisedFilterConfig}
import parsley.token.predicate.Basic

class VisitorTests extends ParsleyTest {
  sealed trait ConstUnit[+A]
  object CUnit extends ConstUnit[Nothing]

  private val testVisitor: LazyParsleyIVisitor[Unit, ConstUnit] =
    new GenericLazyParsleyIVisitor[Unit, ConstUnit] {
      override def visitSingleton[A](self: Singleton[A], context: Unit): ConstUnit[A] = CUnit

      override def visitUnary[A, B](self: Unary[A, B], context: Unit)(p: LazyParsley[A]): ConstUnit[B] = CUnit

      override def visitBinary[A, B, C](self: Binary[A, B, C], context: Unit)(l: LazyParsley[A], r: => LazyParsley[B]): ConstUnit[C] = CUnit

      override def visitTernary[A, B, C, D](self: Ternary[A, B, C, D], context: Unit)(f: LazyParsley[A], s: => LazyParsley[B], t: => LazyParsley[C]): ConstUnit[D] = CUnit

      override def visit[A](self: <|>[A])(context: Unit, p: LazyParsley[A], q: LazyParsley[A]): ConstUnit[A] = CUnit

      override def visit[A](self: ChainPre[A], context: Unit)(p: LazyParsley[A], op: => LazyParsley[A => A]): ConstUnit[A] = CUnit
    }

  private def dontExecute(): Nothing =
    fail("Should not execute.")

  private val dummyParser: LazyParsley[Nothing] =
    new LazyParsley[Nothing] {
      override protected def findLetsAux[M[_, _] : ContOps, R](seen: Set[LazyParsley[_]])(implicit state: LetFinderState): M[R, Unit] =
        dontExecute()

        override protected def preprocess[M[_, _] : ContOps, R, A_ >: Nothing](implicit lets: LetMap, recs: RecMap): M[R, StrictParsley[A_]] =
        dontExecute()

      override def visit[T, U[+_]](visitor: LazyParsleyIVisitor[T, U], context: T): U[Nothing] =
        dontExecute()
    }


  private val dummyLabelConfig: LabelConfig = new LabelConfig {
    override private[parsley] def orElse(other: LabelConfig): LabelConfig =
      dontExecute()

    override private[parsley] def orElse(other: LabelWithExplainConfig): LabelWithExplainConfig =
      dontExecute()

    override private[parsley] def asExpectDescs: Iterable[ExpectDesc] =
      dontExecute()

    override private[parsley] def asExpectDescs(otherwise: String): Iterable[ExpectDesc] =
      dontExecute()

    override private[parsley] def asExpectItems(raw: String): Iterable[ExpectItem] =
      dontExecute()

    override private[parsley] def apply[A](p: Parsley[A]): Parsley[A] =
      dontExecute()
  }

  private val dummyCaretWidth: CaretWidth = new FlexibleCaret(0)

  private val dummyErrorBuilder: ErrorBuilder[String] = new DefaultErrorBuilder {
    override def unexpectedToken(cs: Iterable[Char], amountOfInputParserWanted: Int, lexicalError: Boolean): Token =
      dontExecute()
  }

  private def dummySFConfig[A](): SpecialisedFilterConfig[A] = new SpecialisedFilterConfig[A] {
    override private[parsley] def filter(p: Parsley[A])(f: A => Boolean): Parsley[A] =
      dontExecute()

    override private[parsley] def mkError(offset: Int, line: Int, col: Int, caretWidth: Int, x: A): DefuncError =
      dontExecute()

    override private[parsley] def injectLeft[B]: FilterConfig[Either[A, B]] =
      dontExecute()

    override private[parsley] def injectRight[B]: FilterConfig[Either[B, A]] =
      dontExecute()

    override private[parsley] def injectSnd[B]: FilterConfig[(B, A)] =
      dontExecute()
  }

  implicit private class TestVisitorOps[A](p: LazyParsley[A]) {
    def testV: Assertion = p.visit(testVisitor, ()) shouldBe CUnit
  }

  private def dummyRegister(): Reg[Unit] =
    Reg.make[Unit]

  they should "maintain laziness of the parsers visited" in {
    lazy val dontEval: Nothing = fail("Laziness was not maintained.")

    new NewReg(dummyRegister(), dummyParser, dontEval).testV
    new Branch(dummyParser, dontEval, dontEval).testV
    new If(dummyParser, dontEval, dontEval).testV
    new Lift2[Nothing, Nothing, Nothing]((_: Nothing, _: Nothing) => dontEval, dummyParser, dontEval).testV
    new Lift3[Nothing, Nothing, Nothing, Nothing]((_: Nothing, _: Nothing, _: Nothing) => dontEval, dummyParser, dontEval, dontEval).testV
    new Local(dummyRegister(), dummyParser, dontEval).testV
    new <*>(dummyParser, dontEval).testV
    new *>(dummyParser, dontEval).testV
    new <*(dummyParser, dontEval).testV
    new ChainPost(dummyParser, dontEval).testV
    new Chainl(dummyParser, dontEval, dontEval).testV
    new Chainr[Nothing, Nothing](dummyParser, dontEval, (_: Nothing) => dontEval).testV
    new SepEndBy1(dummyParser, dontEval).testV
  }

  they should "all return the constant unit object from the test visitor" in {
    // The lazy parsers have been tested for this in the laziness preservation test.
    new Pure(()).testV
    new Fresh(()).testV
    new Satisfy(_ => true, dummyLabelConfig).testV
    Line.testV
    Col.testV
    Offset.testV
    new Get(dummyRegister()).testV
    new WhiteSpace(_ => true, SpaceDesc.plain, new ErrorConfig).testV
    new SkipComments(SpaceDesc.plain, new ErrorConfig).testV
    new Comment(SpaceDesc.plain, new ErrorConfig).testV
    new Sign(Sign.CombinedType, PlusSignPresence.Optional).testV
    new NonSpecific("foo", identity[String], _ => true, _ => true, _ => false).testV
    new CharTok(' ', dummyLabelConfig).testV
    new SupplementaryCharTok(0, dummyLabelConfig).testV
    new StringTok("bar", dummyLabelConfig).testV
    Eof.testV
    new UniSatisfy(_ => true, dummyLabelConfig).testV
    new Modify(dummyRegister(), identity[Unit]).testV
    Parsley.empty.internal.testV
    new Fail(dummyCaretWidth).testV
    new Unexpected("qux", dummyCaretWidth).testV
    new EscapeMapped(Trie.empty[Int], Set("quux")).testV
    new EscapeAtMost(0, 0).testV
    new EscapeOneOfExactly(0, Nil, dummySFConfig[Int]()).testV
    new SoftKeyword("corge", Basic(_ => true), false, dummyLabelConfig, "grault").testV
    new SoftOperator("garply", Basic(_ => true), Trie.empty[Unit], dummyLabelConfig, "waldo").testV
    new Attempt(dummyParser).testV
    new Look(dummyParser).testV
    new NotFollowedBy(dummyParser).testV
    new Put(dummyRegister(), dummyParser).testV
    new Debug(dummyParser, "fred", false, FullBreak).testV
    new DebugError(dummyParser, "plugh", false, dummyErrorBuilder).testV
    new Filter[Nothing](dummyParser, (_: Nothing) => true).testV
    new MapFilter[Nothing, Nothing](dummyParser, (_: Nothing) => None).testV
    new FilterOut[Nothing](dummyParser, { case _ => "xyzzy" }).testV
    new GuardAgainst[Nothing](dummyParser, { case _ => Seq("thud") }).testV
    new UnexpectedWhen[Nothing](dummyParser, { case _ => ("grunt", None) })
    new <|>(dummyParser, dummyParser).testV
    new >>=[Nothing, Nothing](dummyParser, (_: Nothing) => dummyParser).testV
    new Many(dummyParser).testV
    new SkipMany(dummyParser).testV
    new ManyUntil(dummyParser).testV
    new SkipManyUntil(dummyParser).testV
    new ErrorLabel(dummyParser, Seq("bazola")).testV
    new ErrorExplain(dummyParser, "ztesch").testV
    new ErrorAmend(dummyParser, false).testV
    new ErrorEntrench(dummyParser).testV
    new ErrorDislodge(0, dummyParser).testV
    new ErrorLexical(dummyParser).testV
    new VerifiedError[Nothing](dummyParser, Left((_: Nothing) => Seq("fum")))
  }
}

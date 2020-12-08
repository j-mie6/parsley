package parsley.internal.deepembedding

import parsley.TokenParser.TokenSet
import Sign._
import ContOps._
import parsley.internal.{instructions, UnsafeOption}
import scala.language.higherKinds

private [parsley] class WhiteSpace(ws: TokenSet, start: String, end: String, line: String, nested: Boolean) extends Parsley[Unit]
{
    override def preprocess[Cont[_, _], U >: Unit](implicit seen: Set[Parsley[_]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[U]] = result(this)
    override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] = result(())
    override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
    {
        result(instrs += new instructions.TokenWhiteSpace(ws, start, end, line, nested))
    }
    override def prettyASTAux[Cont[_, _]](implicit ops: ContOps[Cont]): Cont[String, String] = result("whiteSpace")
}

private [parsley] class SkipComments(start: String, end: String, line: String, nested: Boolean) extends Parsley[Unit]
{
    override def preprocess[Cont[_, _], U >: Unit](implicit seen: Set[Parsley[_]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[U]] = result(this)
    override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] = result(())
    override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
    {
        result(instrs += new instructions.TokenSkipComments(start, end, line, nested))
    }
    override def prettyASTAux[Cont[_, _]](implicit ops: ContOps[Cont]): Cont[String, String] = result("skipComments")
}

private [parsley] class Comment(start: String, end: String, line: String, nested: Boolean) extends Parsley[Unit]
{
    override def preprocess[Cont[_, _], U >: Unit](implicit seen: Set[Parsley[_]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[U]] = result(this)
    override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] = result(())
    override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
    {
        result(instrs += new instructions.TokenComment(start, end, line, nested))
    }
    override def prettyASTAux[Cont[_, _]](implicit ops: ContOps[Cont]): Cont[String, String] = result("comment")
}

private [parsley] class Sign[A](ty: SignType, val expected: UnsafeOption[String] = null) extends Parsley[A => A]
{
    override def preprocess[Cont[_, _], F >: A => A](implicit seen: Set[Parsley[_]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[F]] =
    {
        if (label == null) result(this)
        else result(new Sign(ty, label))
    }
    override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] = result(())
    override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
    {
        result(instrs += new instructions.TokenSign(ty, expected))
    }
    override def prettyASTAux[Cont[_, _]](implicit ops: ContOps[Cont]): Cont[String, String] = result("sign")
}

private [parsley] class Natural(val expected: UnsafeOption[String] = null) extends Parsley[Int]
{
    override def preprocess[Cont[_, _], I >: Int](implicit seen: Set[Parsley[_]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[I]] =
    {
        if (label == null) result(this)
        else result(new Natural(label))
    }
    override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] = result(())
    override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
    {
        result(instrs += new instructions.TokenNatural(expected))
    }
    override def prettyASTAux[Cont[_, _]](implicit ops: ContOps[Cont]): Cont[String, String] = result("natural")
}

private [parsley] class Float(val expected: UnsafeOption[String] = null) extends Parsley[Double]
{
    override def preprocess[Cont[_, _], D >: Double](implicit seen: Set[Parsley[_]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[D]] =
    {
        if (label == null) result(this)
        else result(new Float(label))
    }
    override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] = result(())
    override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
    {
        result(instrs += new instructions.TokenFloat(expected))
    }
    override def prettyASTAux[Cont[_, _]](implicit ops: ContOps[Cont]): Cont[String, String] = result("float")
}

private [parsley] class Escape(val expected: UnsafeOption[String] = null) extends Parsley[Char]
{
    override def preprocess[Cont[_, _], C >: Char](implicit seen: Set[Parsley[_]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[C]] =
    {
        if (label == null) result(this)
        else result(new Escape(label))
    }
    override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] = result(())
    override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
    {
        result(instrs += new instructions.TokenEscape(expected))
    }
    override def prettyASTAux[Cont[_, _]](implicit ops: ContOps[Cont]): Cont[String, String] = result("escape")
}

private [parsley] class StringLiteral(ws: TokenSet, val expected: UnsafeOption[String] = null) extends Parsley[String]
{
    override def preprocess[Cont[_, _], S >: String](implicit seen: Set[Parsley[_]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[S]] =
    {
        if (label == null) result(this)
        else result(new StringLiteral(ws, label))
    }
    override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] = result(())
    override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
    {
        result(instrs += new instructions.TokenString(ws, expected))
    }
    override def prettyASTAux[Cont[_, _]](implicit ops: ContOps[Cont]): Cont[String, String] = result("stringLiteral")
}

private [parsley] class RawStringLiteral(val expected: UnsafeOption[String] = null) extends Parsley[String]
{
    override def preprocess[Cont[_, _], S >: String](implicit seen: Set[Parsley[_]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[S]] =
    {
        if (label == null) result(this)
        else result(new RawStringLiteral(label))
    }
    override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] = result(())
    override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
    {
        result(instrs += new instructions.TokenRawString(expected))
    }
    override def prettyASTAux[Cont[_, _]](implicit ops: ContOps[Cont]): Cont[String, String] = result("rawStringLiteral")
}

private [parsley] class Identifier(start: TokenSet, letter: TokenSet, keywords: Set[String], val expected: UnsafeOption[String] = null) extends Parsley[String]
{
    override def preprocess[Cont[_, _], S >: String](implicit seen: Set[Parsley[_]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[S]] =
    {
        if (label == null) result(this)
        else result(new Identifier(start, letter, keywords, label))
    }
    override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] = result(())
    override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
    {
        result(instrs += new instructions.TokenIdentifier(start, letter, keywords, expected))
    }
    override def prettyASTAux[Cont[_, _]](implicit ops: ContOps[Cont]): Cont[String, String] = result("identifier")
}

private [parsley] class UserOp(start: TokenSet, letter: TokenSet, operators: Set[String], val expected: UnsafeOption[String] = null) extends Parsley[String]
{
    override def preprocess[Cont[_, _], S >: String](implicit seen: Set[Parsley[_]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[S]] =
    {
        if (label == null) result(this)
        else result(new UserOp(start, letter, operators, label))
    }
    override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] = result(())
    override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
    {
        result(instrs += new instructions.TokenUserOperator(start, letter, operators, expected))
    }
    override def prettyASTAux[Cont[_, _]](implicit ops: ContOps[Cont]): Cont[String, String] = result("userOp")
}

private [parsley] class ReservedOp(start: TokenSet, letter: TokenSet, operators: Set[String], val expected: UnsafeOption[String] = null) extends Parsley[String]
{
    override def preprocess[Cont[_, _], S >: String](implicit seen: Set[Parsley[_]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[S]] =
    {
        if (label == null) result(this)
        else result(new ReservedOp(start, letter, operators, label))
    }
    override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] = result(())
    override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
    {
        result(instrs += new instructions.TokenOperator(start, letter, operators, expected))
    }
    override def prettyASTAux[Cont[_, _]](implicit ops: ContOps[Cont]): Cont[String, String] = result("reservedOp")
}

private [parsley] class Keyword(private [Keyword] val keyword: String, letter: TokenSet, caseSensitive: Boolean, val expected: UnsafeOption[String] = null) extends Parsley[Unit]
{
    override def preprocess[Cont[_, _], U >: Unit](implicit seen: Set[Parsley[_]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[U]] =
    {
        if (label == null) result(this)
        else result(new Keyword(keyword, letter, caseSensitive, label))
    }
    override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] = result(())
    override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
    {
        result(instrs += new instructions.TokenKeyword(keyword, letter, caseSensitive, expected))
    }
    override def prettyASTAux[Cont[_, _]](implicit ops: ContOps[Cont]): Cont[String, String] = result(s"keyword($keyword)")
}

private [parsley] class Operator(private [Operator] val operator: String, letter: TokenSet, val expected: UnsafeOption[String] = null) extends Parsley[Unit]
{
    override def preprocess[Cont[_, _], U >: Unit](implicit seen: Set[Parsley[_]], sub: SubMap, label: UnsafeOption[String], ops: ContOps[Cont]): Cont[Parsley[_], Parsley[U]] =
    {
        if (label == null) result(this)
        else result(new Operator(operator, letter, label))
    }
    override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] = result(())
    override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops: ContOps[Cont]): Cont[Unit, Unit] =
    {
        result(instrs += new instructions.TokenOperator_(operator, letter, expected))
    }
    override def prettyASTAux[Cont[_, _]](implicit ops: ContOps[Cont]): Cont[String, String] = result(s"operator($operator)")
}

private [parsley] class MaxOp(private [MaxOp] val operator: String, ops: Set[String], val expected: UnsafeOption[String] = null) extends Parsley[Unit]
{
    override def preprocess[Cont[_, _], U >: Unit](implicit seen: Set[Parsley[_]], sub: SubMap, label: UnsafeOption[String], ops_ : ContOps[Cont]): Cont[Parsley[_], Parsley[U]] =
    {
        if (label == null) result(this)
        else result(new MaxOp(operator, ops, label))
    }
    override def findLetsAux[Cont[_, _]](implicit seen: Set[Parsley[_]], state: LetFinderState, ops: ContOps[Cont]): Cont[Unit, Unit] = result(())
    override def codeGen[Cont[_, _]](implicit instrs: InstrBuffer, state: CodeGenState, ops_ : ContOps[Cont]): Cont[Unit, Unit] =
    {
        result(instrs += new instructions.TokenMaxOp(operator, ops, expected))
    }
    override def prettyASTAux[Cont[_, _]](implicit ops: ContOps[Cont]): Cont[String, String] = result(s"maxOp($operator)")
}

private [parsley] object Sign
{
    private [parsley] sealed trait SignType
    {
        type resultType
    }
    private [parsley] case object DoubleType extends SignType
    {
        override type resultType = Double
    }
    private [parsley] case object IntType extends SignType
    {
        override type resultType = Int
    }
}

private [deepembedding] object Keyword
{
    def unapply(self: Keyword): Option[String] = Some(self.keyword)
}
private [deepembedding] object Operator
{
    def unapply(self: Operator): Option[String] = Some(self.operator)
}
private [deepembedding] object MaxOp
{
    def unapply(self: MaxOp): Option[String] = Some(self.operator)
}
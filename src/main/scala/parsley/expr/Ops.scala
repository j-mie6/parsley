package parsley.expr

import parsley.Parsley

trait Ops[-A, B] {
    private [expr] val wrap: A => B
}
private [expr] case class Lefts[-A, B](ops: Parsley[(B, A) => B]*)(override val wrap: A => B) extends Ops[A, B]
private [expr] case class Rights[-A, B](ops: Parsley[(A, B) => B]*)(override val wrap: A => B) extends Ops[A, B]
private [expr] case class Prefixes[-A, B](ops: Parsley[B => B]*)(override val wrap: A => B) extends Ops[A, B]
private [expr] case class Postfixes[-A, B](ops: Parsley[B => B]*)(override val wrap: A => B) extends Ops[A, B]

object GOps {
    def apply[A, B](fixity: Fixity)(ops: Parsley[fixity.GOp[A, B]]*)(implicit wrap: A => B): Ops[A, B] = fixity match {
        case InfixL  => Lefts[A, B](ops.asInstanceOf[Seq[Parsley[InfixL.GOp[A, B]]]]: _*)(wrap)
        case InfixR  => Rights[A, B](ops.asInstanceOf[Seq[Parsley[InfixR.GOp[A, B]]]]: _*)(wrap)
        case Prefix  => Prefixes[A, B](ops.asInstanceOf[Seq[Parsley[Prefix.GOp[A, B]]]]: _*)(wrap)
        case Postfix => Postfixes[A, B](ops.asInstanceOf[Seq[Parsley[Postfix.GOp[A, B]]]]: _*)(wrap)
    }
}

object Ops {
    def apply[A](fixity: Fixity)(ops: Parsley[fixity.Op[A]]*): Ops[A, A] = GOps[A, A](fixity)(ops: _*)
}
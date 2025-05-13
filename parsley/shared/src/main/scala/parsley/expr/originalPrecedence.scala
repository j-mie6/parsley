package parsley.expr

import parsley.Parsley
import parsley.combinator.choice

object originalPrecedence {
    def apply[A](atom0: Parsley[A], atoms: Parsley[A]*)(lvlTightest: OriginalOps[A, A], lvls: OriginalOps[A, A]*): Parsley[A] = {
        apply(lvls.foldLeft[OriginalPrec[A]](new OriginalLevel(OriginalAtoms(atom0, atoms: _*), lvlTightest))(new OriginalLevel(_, _)))
    }

    def apply[A](lvlWeakest: OriginalOps[A, A], lvls: OriginalOps[A, A]*)(atom0: Parsley[A], atoms: Parsley[A]*): Parsley[A] = {
        val (lvlTightest +: lvls_) = (lvlWeakest +: lvls).reverse: @unchecked
        apply(atom0, atoms: _*)(lvlTightest, lvls_ : _*)
    }

    def apply[A](table: OriginalPrec[A]): Parsley[A] = crushLevels(table)

    private def crushLevels[A](lvls: OriginalPrec[A]): Parsley[A] = lvls match {
        case OriginalAtoms(atom0, atoms @ _*) => choice((atom0 +: atoms): _*)
        case OriginalLevel(lvls, ops) => ops.chain(crushLevels(lvls))
    }
}

sealed abstract class OriginalPrec[+A] private [expr] {
    final def :+[Aʹ >: A, B](ops: OriginalOps[Aʹ, B]): OriginalPrec[B] = new OriginalLevel(this, ops)
    final def +:[Aʹ >: A, B](ops: OriginalOps[Aʹ, B]): OriginalPrec[B] = new OriginalLevel(this, ops)
}
private [expr] case class OriginalLevel[A, B](lvls: OriginalPrec[A], ops: OriginalOps[A, B]) extends OriginalPrec[B]

case class OriginalAtoms[+A](atom0: Parsley[A], atoms: Parsley[A]*) extends OriginalPrec[A]

abstract class OriginalOps[-A, B] {
    private [expr] def chain(p: Parsley[A]): Parsley[B]
}

object OriginalOps {
    def apply[A](fixity: Fixity)(op0: Parsley[fixity.Op[A, A]], ops: Parsley[fixity.Op[A, A]]*): OriginalOps[A, A] = OriginalGOps[A, A](fixity)(op0, ops: _*)
}

object OriginalGOps {
    def apply[A, B](fixity: Fixity)(op0: Parsley[fixity.Op[A, B]], ops: Parsley[fixity.Op[A, B]]*)(implicit wrap: A => B): OriginalOps[A, B] = new OriginalOps[A, B] {
        private [expr] def chain(p: Parsley[A]): Parsley[B] = fixity.chain(p, choice((op0 +: ops): _*))
    }
}

object OriginalSOps {
    def apply[B, A <: B](fixity: Fixity)(op0: Parsley[fixity.Op[A, B]], ops: Parsley[fixity.Op[A, B]]*): OriginalOps[A, B] = OriginalGOps(fixity)(op0, ops: _*)
}

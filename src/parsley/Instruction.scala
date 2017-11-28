package parsley

trait Instruction
{
    def apply(ctx: Context): Context
}

case class Perform[-A, +B](f: A => B) extends Instruction
{
    // Not exactly sure what happens here yet
    override def apply(ctx: Context): Context = ctx match
    {
        case ((x: A)::ctx, ib, pc) => (f(x)::ctx, ib, pc)
    }

    override def toString(): String = "Perform(f)"
}

case class Push[A](x: A) extends Instruction
{
    override def apply(ctx: Context): Context = ctx match
    {
        case (ctx, ib, pc) => (x::ctx, ib, pc)
    }
}

case object Pop extends Instruction
{
    override def apply(ctx: Context): Context = ctx match
    {
        case (_::ctx, ib, pc) => (ctx, ib, pc)
    }
}


case object Apply extends Instruction
{
    override def apply(ctx: Context): Context = ctx match
    {
        case (x::f::ctx, ib, pc) => ((f.asInstanceOf[Function[A forSome {type A}, B forSome {type B}]])(x)::ctx, ib, pc)
    }
}

case class Call(x: String) extends Instruction
{
    override def apply(ctx: Context): Context = ???
}

case class DynSub[-A](f: A => Vector[Instruction]) extends Instruction
{
    override def apply(ctx: Context): Context = ctx match
    {
        case ((x: A)::ctx, ib, pc) => (ctx, f(x)::ib, 0)
    }
}

case class Fail(msg: String) extends Instruction
{
    override def apply(ctx: Context): Context = ctx match
    {
        case _ => ???
    }
}
//case class Many(x: Int) extends Instruction

// What even is this?
// case class Choice

//case class Label(x: Int) extends Instruction

object Instruction
{
    def main(args: Array[String]): Unit =
    {
        //println(Apply(Perform[Int, Int=>Int](x => y => x + y)(Push(10)(List(20), Nil, 0))))
        println(Apply(Push(20)(Perform[Int, Int=>Int](x => y => x + y)(Push(10)(Nil, Nil, 0)))))
        println(Apply(Push(20)(Apply(Push(10)(Push[Int=>Int=>Int](x => y => x + y)(Nil, Nil, 0))))))
    }
}
package parsley

import language.existentials

trait Instruction
{
    def apply(ctx: Context): Context
}

case class Perform[-A, +B](f: A => B) extends Instruction
{
    // Not exactly sure what happens here yet
    override def apply(ctx: Context): Context = ctx match
    {
        case ctx if ctx.failed => ctx.copy(pc = ctx.pc + 1)
        case Context((x: A @unchecked)::stack, is, inputs, checks, subs, failed, pc) => Context(f(x)::stack, is, inputs, checks, subs, failed, pc + 1)
        case Context(Nil, _, _, _, _, _, _) => ??? //failure
    }

    override def toString: String = "Perform(f)"
}

case class Push[A](x: A) extends Instruction
{
    override def apply(ctx: Context): Context = ctx.copy(stack = if (ctx.failed) ctx.stack else x::ctx.stack, pc = ctx.pc + 1)
}

case object Pop extends Instruction
{
    override def apply(ctx: Context): Context = ctx.copy(stack = if (ctx.failed) ctx.stack else ctx.stack.tail, pc = ctx.pc + 1)
}


case object Apply extends Instruction
{
    override def apply(ctx: Context): Context = ctx match
    {
        case ctx if ctx.failed => ctx.copy(pc = ctx.pc + 1)
        case Context(x::f::stack, is, inputs, checks, subs, failed, pc) =>
            Context(f.asInstanceOf[Function[A forSome {type A}, B forSome {type B}]](x)::stack, is, inputs, checks, subs, failed, pc+1)
        case Context(_, _, _, _, _, _, _) => ??? //failure
    }
}

case class Call(x: String) extends Instruction
{
    // TODO: Not sure how failure fits in here?
    override def apply(ctx: Context): Context = ctx.copy(instrss = ctx.subs(x)::ctx.instrss, pc = 0)
}

case class DynSub[-A](f: A => Vector[Instruction]) extends Instruction
{
    override def apply(ctx: Context): Context = ctx match
    {
        case ctx if ctx.failed => ctx.copy(pc = ctx.pc + 1)
        case Context((x: A @unchecked)::stack, is, inputs, checks, subs, failed, pc) => Context(stack, f(x)::is, inputs, checks, subs, failed, 0)
        case Context(Nil, _, _, _, _, _, _) => ??? //failure
    }
}

case class Fail(msg: String) extends Instruction
{
    override def apply(ctx: Context): Context = ctx.copy(failed = true, pc = ctx.pc + 1)
}

case object TryBegin extends Instruction
{
    override def apply(ctx: Context): Context = ???
}

case object TryEnd extends Instruction
{
    override def apply(ctx: Context): Context = ???
}

//case class Many(x: Int) extends Instruction
case object InputCheck extends Instruction
{
    override def apply(ctx: Context): Context = ???
}

case class JumpGood(label: Int) extends Instruction
{
    override def apply(ctx: Context): Context = ???
}

// This should mostly not affect the state of the interpreter
case class Label(x: Int) extends Instruction
{
    override def apply(ctx: Context): Context = ctx
}

case class CharTok(c: Char) extends Instruction
{
    override def apply(ctx: Context): Context = ctx
}

case class Satisfies(f: Char => Boolean) extends Instruction
{
    override def apply(ctx: Context): Context = ctx
}

case class StringTok(s: String) extends Instruction
{
    override def apply(ctx: Context): Context = ctx
}

object InstructionTests
{
    def main(args: Array[String]): Unit =
    {
        println(Apply(Push(20)(Perform[Int, Int=>Int](x => y => x + y)(Push(10)(Context(Nil, Nil, Nil, Nil, Map.empty, false, 0))))))
        println(Apply(Push(20)(Apply(Push(10)(Push[Int=>Int=>Int](x => y => x + y)(Context(Nil, Nil, Nil, Nil, Map.empty, false, 0)))))))
    }
}
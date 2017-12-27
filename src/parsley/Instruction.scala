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
        case ctx if ctx.failed > 0 => ctx.copy(pc = ctx.pc + 1)
        case Context((x: A @unchecked)::stack, is, inputs, checks, subs, failed, pc) => Context(f(x)::stack, is, inputs, checks, subs, failed, pc + 1)
        case Context(Nil, _, _, _, _, _, _) => ??? //failure
    }

    override def toString: String = "Perform(f)"
}

case class Push[A](x: A) extends Instruction
{
    override def apply(ctx: Context): Context = ctx.copy(stack = if (ctx.failed > 0) ctx.stack else x::ctx.stack, pc = ctx.pc + 1)
}

case object Pop extends Instruction
{
    override def apply(ctx: Context): Context = ctx.copy(stack = if (ctx.failed > 0) ctx.stack else ctx.stack.tail, pc = ctx.pc + 1)
}


case object Apply extends Instruction
{
    override def apply(ctx: Context): Context = ctx match
    {
        case ctx if ctx.failed > 0 => ctx.copy(pc = ctx.pc + 1)
        case Context(x::f::stack, is, inputs, checks, subs, failed, pc) =>
            Context(f.asInstanceOf[Function[A forSome {type A}, B forSome {type B}]](x)::stack, is, inputs, checks, subs, failed, pc+1)
        case Context(_, _, _, _, _, _, _) => ??? //failure
    }
}

case class Call(x: String) extends Instruction
{
    // TODO: Not sure how failure fits in here?
    override def apply(ctx: Context): Context = ctx.copy(instrss = (ctx.subs(x) :+ Return(ctx.pc))::ctx.instrss, pc = 0)
}

case class DynSub[-A](f: A => Vector[Instruction]) extends Instruction
{
    override def apply(ctx: Context): Context = ctx match
    {
        case ctx if ctx.failed > 0 => ctx.copy(pc = ctx.pc + 1)
        case Context((x: A @unchecked)::stack, is, inputs, checks, subs, failed, pc) =>
            Context(stack, (f(x) :+ Return(ctx.pc))::is, inputs, checks, subs, failed, 0)
        case Context(Nil, _, _, _, _, _, _) => ??? //failure
    }
}

case class Fail(msg: String) extends Instruction
{
    override def apply(ctx: Context): Context = ctx.copy(failed = ctx.failed + 1, pc = ctx.pc + 1)
}

case object TryBegin extends Instruction
{
    override def apply(ctx: Context): Context = ctx.copy(pc = ctx.pc + 1)
}

case object TryEnd extends Instruction
{
    override def apply(ctx: Context): Context = ctx.copy(pc = ctx.pc + 1)
}

//NOTE: If we implement this instruction in the future, we cannot use labels with it
//      for optimisation purposes all labels are forward jumps this means we can
//      resolve all label addresses in a single backwards pass during optimisation.
//      Instead, we need to ensure that we calculate the backwards offset immediately
//      at compile-time.
//case class Many(x: Int) extends Instruction

case object InputCheck extends Instruction
{
    override def apply(ctx: Context): Context = ctx.copy(pc = ctx.pc + 1)
}

case class JumpGood(label: Int) extends Instruction
{
    // We need to assume that the line number was resolved
    // TODO: Handle input checks on failure!!! Expand this out into a bigger function, don't ternary
    override def apply(ctx: Context): Context = ctx.copy(pc = if (ctx.failed > 0) ctx.pc + 1 else label)
}

case class Return(ret: Int) extends Instruction
{
    override def apply(ctx: Context): Context = ctx.copy(pc = ret+1, instrss = ctx.instrss.tail)
}

// This should mostly not affect the state of the interpreter
case class Label(x: Int) extends Instruction
{
    override def apply(ctx: Context): Context = ctx.copy(pc = ctx.pc + 1)
}

case class CharTok(c: Char) extends Instruction
{
    override def apply(ctx: Context): Context = ctx.copy(pc = ctx.pc + 1)
}

case class Satisfies(f: Char => Boolean) extends Instruction
{
    override def apply(ctx: Context): Context = ctx.copy(pc = ctx.pc + 1)
}

case class StringTok(s: String) extends Instruction
{
    override def apply(ctx: Context): Context = ctx.copy(pc = ctx.pc + 1)
}

object InstructionTests
{
    def main(args: Array[String]): Unit =
    {
        println(Apply(Push(20)(Perform[Int, Int=>Int](x => y => x + y)(Push(10)(Context(Nil, Nil, Nil, Nil, Map.empty, 0, 0))))))
        println(Apply(Push(20)(Apply(Push(10)(Push[Int=>Int=>Int](x => y => x + y)(Context(Nil, Nil, Nil, Nil, Map.empty, 0, 0)))))))
    }
}
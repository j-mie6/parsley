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
        case Context((x: A @unchecked)::stack, is, inputs, checks, subs, failed, handlers, pc) => Context(f(x)::stack, is, inputs, checks, subs, failed, handlers, pc + 1)
        case Context(Nil, _, _, _, _, _, _, _) => ??? //failure
    }

    override def toString: String = "Perform(f)"
}

case class Push[A](x: A) extends Instruction
{
    override def apply(ctx: Context): Context = ctx.copy(stack = x::ctx.stack, pc = ctx.pc + 1)
}

case object Pop extends Instruction
{
    override def apply(ctx: Context): Context = ctx.copy(stack = ctx.stack.tail, pc = ctx.pc + 1)
}


case object Apply extends Instruction
{
    override def apply(ctx: Context): Context = ctx match
    {
        case Context(x::f::stack, is, inputs, checks, subs, failed, handlers, pc) =>
            Context(f.asInstanceOf[Function[A forSome {type A}, B forSome {type B}]](x)::stack, is, inputs, checks, subs, failed, handlers, pc+1)
        case Context(_, _, _, _, _, _, _, _) => ??? //failure
    }
}

case class Call(x: String) extends Instruction
{
    override def apply(ctx: Context): Context = ctx.copy(instrss = (ctx.subs(x) :+ Return(ctx.pc))::ctx.instrss, pc = 0)
}

case class DynSub[-A](f: A => Vector[Instruction]) extends Instruction
{
    override def apply(ctx: Context): Context = ctx match
    {
        case Context((x: A @unchecked)::stack, is, inputs, checks, subs, failed, handlers, pc) =>
            Context(stack, (f(x) :+ Return(ctx.pc))::is, inputs, checks, subs, failed, handlers, 0)
        case Context(Nil, _, _, _, _, _, _, _) => ??? //failure
    }
}


case class FastFail[A](finaliser: A=>String) extends Instruction
{
    override def apply(ctx: Context): Context = Fail(finaliser(ctx.stack.head.asInstanceOf[A]))(ctx.copy(stack = ctx.stack.tail))
}

case class Fail(msg: String) extends Instruction
{
    //FIXME: Failure to be handled with no handlers
    override def apply(ctx: Context): Context =
    {
        val (depth, handler) = ctx.handlers.head
        val handlers = ctx.handlers.tail
        val instrss = ctx.instrss
        val diff = instrss.size - depth
        ctx.copy(failed = true, pc = handler, handlers = handlers, instrss = if (diff > 0) instrss.drop(diff) else instrss)
    }
}

case class TryBegin(handler: Int) extends Instruction
{
    override def apply(ctx: Context): Context = ctx.copy(pc = ctx.pc + 1, handlers = (ctx.instrss.size, handler)::ctx.handlers)
}

case object TryEnd extends Instruction
{
    //TODO: Need restorative properties in case of failure
    override def apply(ctx: Context): Context = ctx.copy(pc = ctx.pc + 1, handlers = ctx.handlers.tail, failed = false)
}

//NOTE: If we implement this instruction in the future, we cannot use labels with it
//      for optimisation purposes all labels are forward jumps this means we can
//      resolve all label addresses in a single backwards pass during optimisation.
//      Instead, we need to ensure that we calculate the backwards offset immediately
//      at compile-time.
//case class Many(x: Int) extends Instruction

// TODO: this could be implemented by failure jumps instead of fall through, is it computable?
case class InputCheck(handler: Int) extends Instruction
{
    override def apply(ctx: Context): Context = ctx.copy(pc = ctx.pc + 1, handlers = (ctx.instrss.size, handler)::ctx.handlers)
}

case class JumpGood(label: Int) extends Instruction
{
    // We need to assume that the line number was resolved
    // TODO: This must change, if input is not consumed, then jump, else fail to next handler
    override def apply(ctx: Context): Context = ??? //ctx.copy(pc = if (ctx.failed > 0) ctx.pc + 1 else label)
}

case class Return(ret: Int) extends Instruction
{
    override def apply(ctx: Context): Context = ctx.copy(pc = ret+1, instrss = ctx.instrss.tail)
}

// This should mostly not affect the state of the interpreter
case class Label(x: Int) extends Instruction
{
    // Labels should not be executed, they SHOULD be removed from the bytecode
    override def apply(ctx: Context): Context = ???
}

case class CharTok(c: Char) extends Instruction
{
    override def apply(ctx: Context): Context = ctx.inputs match
    {
        case input::inputs if input.head == c => ctx.copy(pc = ctx.pc + 1, stack = input.head::ctx.stack, inputs = input.tail::inputs)
        //FIXME: Failure to be handled with no handlers
        case inputs =>
            val (depth, handler) = ctx.handlers.head
            val handlers = ctx.handlers.tail
            val instrss = ctx.instrss
            val diff = instrss.size - depth
            ctx.copy(failed = true, pc = handler, handlers = handlers, instrss = if (diff > 0) instrss.drop(diff) else instrss)
    }
}

case class Satisfies(f: Char => Boolean) extends Instruction
{
    override def apply(ctx: Context): Context = ctx.inputs match
    {
        case input::inputs if f(input.head) => ctx.copy(pc = ctx.pc + 1, stack = input.head::ctx.stack, inputs = input.tail::inputs)
        //FIXME: Failure to be handled with no handlers
        case inputs =>
            val (depth, handler) = ctx.handlers.head
            val handlers = ctx.handlers.tail
            val instrss = ctx.instrss
            val diff = instrss.size - depth
            ctx.copy(failed = true, pc = handler, handlers = handlers, instrss = if (diff > 0) instrss.drop(diff) else instrss)
    }
}

case class StringTok(s: String) extends Instruction
{
    override def apply(ctx: Context): Context = ctx.inputs match
    {
        case input::inputs if input.startsWith(s) => ctx.copy(pc = ctx.pc + 1, stack = input.head::ctx.stack, inputs = input.drop(s.size)::inputs)
        //FIXME: Failure to be handled with no handlers
        case inputs =>
            val (depth, handler) = ctx.handlers.head
            val handlers = ctx.handlers.tail
            val instrss = ctx.instrss
            val diff = instrss.size - depth
            ctx.copy(failed = true, pc = handler, handlers = handlers, instrss = if (diff > 0) instrss.drop(diff) else instrss)
    }
}

object InstructionTests
{
    def main(args: Array[String]): Unit =
    {
        println(Apply(Push(20)(Perform[Int, Int=>Int](x => y => x + y)(Push(10)(Context(Nil, Nil, Nil, Nil, Map.empty, false, Nil, 0))))))
        println(Apply(Push(20)(Apply(Push(10)(Push[Int=>Int=>Int](x => y => x + y)(Context(Nil, Nil, Nil, Nil, Map.empty, false, Nil, 0)))))))
        println(Apply(CharTok('b')(Perform[Char, Char=>String](x => y => x.toString + y.toString)(CharTok('a')(Context(Nil, Nil, List("ab"), Nil, Map.empty, false, Nil, 0))))))
    }
}
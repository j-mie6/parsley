package parsley

import language.existentials
import scala.annotation.tailrec
import scala.collection.mutable.Buffer

trait Instruction
{
    def apply(ctx: Context): Context
}

case class Perform[-A, +B](f: A => B) extends Instruction
{
    val g = f.asInstanceOf[Function[Any, Any]] 
    override def apply(ctx: Context): Context = ctx.copy(stack = g(ctx.stack.head)::ctx.stack.tail, pc = ctx.pc + 1)
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

case class Exchange[A](x: A) extends Instruction
{
    override def apply(ctx: Context): Context = ctx.copy(stack = x::ctx.stack.tail, pc = ctx.pc + 1)
}

case object Apply extends Instruction
{
    override def apply(ctx: Context): Context =
    {
        val f = ctx.stack.tail.head.asInstanceOf[Function[A forSome {type A}, B forSome {type B}]]
        ctx.copy(stack = f(ctx.stack.head)::ctx.stack.tail.tail, pc = ctx.pc + 1)
    }
}

// Unlike lift, this instruction is beneficial, but we'll wait to use it!
case object Cons extends Instruction
{
    override def apply(ctx: Context): Context = ctx.copy(stack = (ctx.stack.tail.head::ctx.stack.head.asInstanceOf[List[_]])::ctx.stack.tail.tail, pc = ctx.pc + 1)
}

// This instruction seems to be slower than f <#> p <*> q, how strange!
/*case class Lift[-A, -B, +C](f: (A, B) => C) extends Instruction
{
    val g = f.asInstanceOf[(Any, Any) => Any]
    override def apply(ctx: Context): Context = ctx.copy(stack = g(ctx.stack.tail.head, ctx.stack.head)::ctx.stack.tail.tail, pc = ctx.pc + 1)
    override def toString: String = "Lift(f)"
}*/

case class Call(x: String) extends Instruction
{
    override def apply(ctx: Context): Context = ctx.copy(instrss = (ctx.subs(x) :+ Return(ctx.pc))::ctx.instrss, pc = 0)
}

case class DynSub[-A](f: A => Buffer[Instruction]) extends Instruction
{
    val g = f.asInstanceOf[Any => Buffer[Instruction]]
    override def apply(ctx: Context): Context = ctx.copy(stack = ctx.stack.tail, instrss = (g(ctx.stack.head) :+ Return(ctx.pc))::ctx.instrss, pc = 0)
}

case class FastFail[A](msggen: A=>String) extends Instruction
{
    val msggen_ = msggen.asInstanceOf[Any => String]
    override def apply(ctx: Context): Context = Fail(msggen_(ctx.stack.head))(ctx.copy(stack = ctx.stack.tail))
}

case class Fail(msg: String) extends Instruction
{
    override def apply(ctx: Context): Context =
    {
        if (ctx.handlers.isEmpty) return ctx.copy(status = Failed)
        val (depth, handler) = ctx.handlers.head
        val handlers = ctx.handlers.tail
        val instrss = ctx.instrss
        val diff = instrss.size - depth
        ctx.copy(status = Recover, pc = handler, handlers = handlers, instrss = if (diff > 0) instrss.drop(diff) else instrss)
    }
}

case class TryBegin(handler: Int) extends Instruction
{
    override def apply(ctx: Context): Context = ctx.copy(pc = ctx.pc + 1, handlers = (ctx.instrss.size, handler)::ctx.handlers)
}

case object TryEnd extends Instruction
{
    //TODO: Need restorative properties in case of failure
    override def apply(ctx: Context): Context = ctx.copy(pc = ctx.pc + 1, handlers = ctx.handlers.tail, status = Good)
}

//NOTE: If we implement this instruction in the future, we cannot use labels with it
//      for optimisation purposes all labels are forward jumps this means we can
//      resolve all label addresses in a single backwards pass during optimisation.
//      Instead, we need to ensure that we calculate the backwards offset immediately
//      at compile-time.
//case class Many(x: Int) extends Instruction

case class InputCheck(handler: Int) extends Instruction
{
    override def apply(ctx: Context): Context = ctx.copy(pc = ctx.pc + 1, handlers = (ctx.instrss.size, handler)::ctx.handlers)
}

case class JumpGood(label: Int) extends Instruction
{
    // We need to assume that the line number was resolved
    // TODO: This must change, if input is not consumed, then jump, else fail to next handler
    override def apply(ctx: Context): Context = ??? //ctx.copy(pc = if (ctx.status > 0) ctx.pc + 1 else label)
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
        case (`c`::input)::inputs => ctx.copy(pc = ctx.pc + 1, stack = c::ctx.stack, inputs = input::inputs)
        case inputs =>
            if (ctx.handlers.isEmpty) return ctx.copy(status = Failed)
            val (depth, handler) = ctx.handlers.head
            val handlers = ctx.handlers.tail
            val instrss = ctx.instrss
            val diff = instrss.size - depth
            ctx.copy(status = Recover, pc = handler, handlers = handlers, instrss = if (diff > 0) instrss.drop(diff) else instrss)
    }
}

case class Satisfies(f: Char => Boolean) extends Instruction
{
    override def apply(ctx: Context): Context = ctx.inputs match
    {
        case (c::input)::inputs if f(c) => ctx.copy(pc = ctx.pc + 1, stack = c::ctx.stack, inputs = input::inputs)
        case inputs =>
            if (ctx.handlers.isEmpty) return ctx.copy(status = Failed)
            val (depth, handler) = ctx.handlers.head
            val handlers = ctx.handlers.tail
            val instrss = ctx.instrss
            val diff = instrss.size - depth
            ctx.copy(status = Recover, pc = handler, handlers = handlers, instrss = if (diff > 0) instrss.drop(diff) else instrss)
    }
}

case class StringTok(s: String) extends Instruction
{
    val ls = s.toList
    override def apply(ctx: Context): Context = ctx.inputs match
    {
        case input::inputs if input.startsWith(ls) => ctx.copy(pc = ctx.pc + 1, stack = s::ctx.stack, inputs = input.drop(s.size)::inputs)
        case inputs =>
            if (ctx.handlers.isEmpty) return ctx.copy(status = Failed)
            val (depth, handler) = ctx.handlers.head
            val handlers = ctx.handlers.tail
            val instrss = ctx.instrss
            val diff = instrss.size - depth
            ctx.copy(status = Recover, pc = handler, handlers = handlers, instrss = if (diff > 0) instrss.drop(diff) else instrss)
    }
}

object InstructionTests
{
    def main(args: Array[String]): Unit =
    {
        println(Apply(Push(20)(Perform[Int, Int=>Int](x => y => x + y)(Push(10)(Context(Nil, Nil, Nil, Nil, Map.empty, Good, Nil, 0))))))
        println(Apply(Push(20)(Apply(Push(10)(Push[Int=>Int=>Int](x => y => x + y)(Context(Nil, Nil, Nil, Nil, Map.empty, Good, Nil, 0)))))))
        import parsley.Parsley._
        //val p = lift2[Char, Char, String]((x, y) => x.toString + y.toString, 'a', 'b')
        val p = 'a' <::> ('b' #> Nil)
        //val p = 'a' *> 'b' #> "ab"
        println(p)
        val q = optimise(p)
        reset()
        println(q)
        println(runParser(q, "ab"))
        val start = System.currentTimeMillis()
        val input = "ab".toList
        for (i <- 0 to 10000000) runParser(q, input)
        println(System.currentTimeMillis() - start)
    }
}
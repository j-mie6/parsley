package parsley

trait Breakpoint
case object NoBreak extends Breakpoint
case object EntryBreak extends Breakpoint
case object ExitBreak extends Breakpoint
case object FullBreak extends Breakpoint
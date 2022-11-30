# Parsley Internals (`parsley.internal.machine`)

> _Here be Dragons!_

Parsley's internal-most machinery is divided up into several parts. These are
_significantly_ more challenging to understand than the higher-level components
in the `deepembedding` package.

## `machine.Context`
The `Context` is the collection of state that underpins the execution of a parser.
It is fed through the execution of the entire machine and exposes many useful
operations required by the instructions. It is made up several pieces of state:

### Basic State
* The input offset (`Context#offset`) is used to track how many characters have been
  read so far.
* The program counter (`Context#pc`) keeps track of which instruction should be
  executed next.
* The source input (`Context#input`), unsurprisingly, is the input to the parser
* The instruction array `Context#instrs`) is the array of instructions that make
  up this parser. The _program counter_ is an index into this array at all times.
* The status (`Context#status`) represents one of four states: `Good`, `Recover`
  `Failed`, and `Finished`. These states are used to direct the flow of control
  through the machine: `Failed` and `Finished` are terminal statuses, and the
  parser will stop immediately; and `Good` and `Recover` denote whether the
  parser recently soft failed or not.
* The call stack depth (`Context#depth`) tracks how deep the _call stack_ is.
  This is required as failure can drop back from multiple stack frames. This
  allows the failure mechanism to track how many it needs to drop on failure.
* The position information (`Context#line` and `Context#col`) track the current
  line and column number in the input. When a tab is encountered, this shifts
  the column number up to the nearest 4th column + 1.
* The register array (`Context#regs`) stores up to `numRegs` registers, which
  can take any type. These are used for stateful persistence in the parser.
* The current in-flight hints (`Context#hints`) represent the "ghosts" of
  old error messages that occured at the same offset that the parser is
  currently at (this implies that the error was recovered from without
  consuming input). These can get added to any error messages generated
  at this same offset.
* The valid offset for hints (`Context#hintsValidOffset`) helps enforce
  the above property, so that the hints can only be added to an error `err` when
  `hintsValidOffset == err.offset`.

### The Stacks
The stacks make up the core part of the machine state. There are **seven** stacks
in total:

1. The operand stack (`Context#stack`) is a general purpose stack where intermediate
   results of the parser go. In addition, some instructions may store other things
   on this stack, as it takes values of `Any` type at all. At the end of execution,
   this stack _must_ have a single item on it, and this is the parsers result.
2. The handler stack (`Context#handlers`) stores failure handlers: these are
   popped off of the stack when the parser fails and are used to recover from that
   failure. If the stack is empty when this happens, the parser fails completely.
   The elements on this stack are triples consisting of the depth of the _call stack_
   where this handler should be executed, the program counter value where the handler
   instruction is situated, and the size of the _operand stack_ when this handler was
   loaded onto the stack. These are required, as the _call stack_ and _operand stack_
   must be popped on failure, and execution switches to the handler instruction
   itself. It should be empty when the parser is complete.
3. The check stack (`Context#checkStack`) is used to record the offset that the
   parser has read up to at a certain point in time. This information is often
   required by failure handlers, as there are many combinators that can only recover
   from failure if the parser has not consumed input on the failing branch: whether
   or not this is the case is determined by comparing the offset on the _check
   stack_ with the current offset of the parser. It should be empty when the parser
   is complete.
4. The state stack (`Context#states`) is similar to the _check stack_, except it
   also records the line and column numbers of the parser as well as the offset.
   This is important for combinators that do backtrack even if input was consumed:
   in these cases, the line and column number is known to have changed and so they
   too must be kept safe on the stack. It should be empty when the parser is
   complete.
5. The call stack (`Context#calls`) is used to handle recursion, execution of shared
   parsers, and dynamic execution of parsers (i.e. arising from `flatMap`). It
   achieves this by recording the return address (i.e. `pc + 1`) as well as the
   current instruction array. For calls to recursive and non-recursive shared
   parsers, the instructions array will not change from call to call; however, a
   dynamic call using `DynCall` will load an entirely new set of instructions into
   the context, so the old ones must also be preserved. It should be empty when the
   parser is complete.
6. The error stack (`Context#errs`) stores error messages as they are produced by
   parser failures. The stack itself will be collapsed primarily by error handlers,
   which will either discard errors, convert them to "hints", or merge them together.
   Other operations may modify the head of this stack, for instance to add
   reasons or error labels to it. When the parser completely fails, there must
   be exactly one error message on the stack, and this is what is returned.
7. The hint stack (`Context#hintStack`) stores hints that are being protected from
   the effects of other combinators temporarily. These may later be merged into the
   active hints, discarded, or replace the active hints. It should be empty when the
   parser is complete.

## `machine.stacks`
The stacks used by `Context` are manually specialised to avoid boxing and reduce
memory allocations. Each is specialised here, however the _operand stack_ is
kept as an `ArrayStack` as it is most frequently used and cannot benefit from
unboxing anyway.

## `machine.instructions`
Within this package are the definitions of all of Parsley's instructions. A more full
description of these instructions is left to the relevant README.

## `machine.errors`
In the above package, `parsley.internal.errors` was advertised as having the
internal representation of error messages: this was a _small_ white lie. In fact,
working that that representation would be wildly inefficient, especially for
successful parsers. Instead this package contains a _defunctionalised_ representation
of error messages. This means that every operation that would be otherwise
performed between two `ParseError`s is now represented as data instead as a
subtype of `DefuncError`. This means that whenever any operation is performed on
error messages, instead of actually performing the operation, the machine allocates
a single defunctionalised operation that can be interpreted later on. In some
cases, these operations might skip the allocation by performing very light-weight
checks, but otherwise, it's mostly just raw allocation. In other words, this is
like having Haskell's laziness to hand.

The advantages of this are that:

* it allows for the inspection of constructed errors: we can actually see _how_ an
  error was built, operation by operation.
* it ensures that if an error message is dropped (which will happen many times
  during a parse), it will just be garbage collected, and unnecessary work didn't
  have to be performed.
* when the errors are actually interpreted into a `ParseError`, this reduction can
  be done more efficiently, given knowledge about the structure and that more
  operations cannot occur: in simpler terms, the evaluation can be optimised using
  mutable collections as well as take otherwise unsound shortcuts. As an example,
  when an error is relabeled, the labels of the underlying error do not need to
  be computed at all: this would not be the case if the `ParseErrors` were
  constructed strictly.

The disadvantages? Basically none as `ParseError` construction would also requires
allocation, however this system does require slightly more memory consumption
per-error (there is less churn with old objects being destroyed and new ones made, instead object trees are kept in memory).

The same system is used for the hints. In this case, the evaluation of hints is
implemented tail-recursively using mutable collections, and when parse errors are
converted into hints, _only_ the work needed to find the expected items is performed,
which again improves performance nicely.

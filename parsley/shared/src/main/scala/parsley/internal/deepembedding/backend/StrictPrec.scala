package parsley.internal.deepembedding.backend

import parsley.expr.Fixity

private [parsley] case class StrictPrec(atoms: List[StrictParsley[Any]], ops: List[StrictOp], wraps: Array[(Any => Any, Boolean)])

private [parsley] case class StrictOp(fixity: Fixity, op: StrictParsley[Any], prec: Int)
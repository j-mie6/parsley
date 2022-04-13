package parsley.internal.deepembedding.singletons

import scala.language.higherKinds

import parsley.internal.deepembedding.ContOps, ContOps.{result, ContAdapter}
import parsley.internal.deepembedding.backend, backend.StrictParsley
import parsley.internal.deepembedding.frontend, frontend.LazyParsley
import parsley.internal.machine.instructions

private [singletons] abstract class Singleton[A] extends LazyParsley[A] with StrictParsley[A] {
    def inlinable: Boolean = true
    def pretty: String
    def instr: instructions.Instr

    final override def findLetsAux[Cont[_, +_], R](seen: Set[LazyParsley[_]])
        (implicit ops: ContOps[Cont], state: frontend.LetFinderState): Cont[R, Unit] = result(())
    final override def preprocess[Cont[_, +_], R, A_ >: A](implicit ops: ContOps[Cont],
                                                                    lets: frontend.LetMap, recs: frontend.RecMap): Cont[R, StrictParsley[A_]] = result(this)
    final override def codeGen[Cont[_, +_], R](implicit ops: ContOps[Cont], instrs: StrictParsley.InstrBuffer, state: backend.CodeGenState): Cont[R, Unit] = {
        result(instrs += instr)
    }
    // $COVERAGE-OFF$
    final override def prettyASTAux[Cont[_, +_]](implicit ops: ContOps[Cont]): Cont[String, String] = result(pretty)
    // $COVERAGE-ON$
}

package parsley.internal.deepembedding.backend

import scala.collection.mutable
import parsley.internal.machine.instructions, instructions.Instr

private [deepembedding] object PreservationAnalysis {
    def determinePreserve(recs: Iterable[Rec[_]], instrs: Array[Instr]): Unit = if (recs.nonEmpty) {
        var start = System.currentTimeMillis()
        implicit val coordinator: EventCoordinator = new EventCoordinator
        val recReactors = mutable.ListBuffer.empty[(Rec[_], DataflowReactor)]
        val reactors = mutable.Map.empty[Int, DataflowReactor]
        val sources = mutable.Map.empty[Int, EventSource[Iterable[Int]]]
        val dependencies = new EventSource[(DataflowReactor, Int)]
        val topo = mutable.ListBuffer.empty[(Int, DataflowReactor)]
        // Build reactors and event streams
        def addToNetwork(label: Int, reactor: DataflowReactor): Unit = {
            reactors(label) = reactor
            for (dependency <- instructions.dependencies(instrs, label)) {
                dependencies.fire((reactor, dependency))
            }
            // Poor mans topological sort, it's not completely accurate
            // to make it accurate we'd have to form a graph in the _other_ direction
            // graph from dependency -> dependent as opposed to dependent -> dependencies
            (label -> reactor) +=: topo
        }
        for (rec <- recs) {
            val source = new EventSource[Iterable[Int]]
            val reactor = new DataflowReactor(rec.label, source)
            recReactors += (rec -> reactor)
            sources(rec.label) = source
            addToNetwork(rec.label, reactor)
        }
        for (relation <- dependencies) {
            val (dependent, label) = relation
            val source = sources.getOrElseUpdate(label, {
                val source = new EventSource[Iterable[Int]]
                addToNetwork(label, new DataflowReactor(label, source))
                source
            })
            source.foreach(dependent)
        }
        // Initialise dataflow
        coordinator.run()
        for ((label, reactor) <- topo) {
            reactor(instructions.statefulIndicesToReturn(instrs, label))
        }
        // Execute
        coordinator.run()
        for ((rec, reactor) <- recReactors) {
            rec.preserve = reactor.preserve
        }
    }
}

private [deepembedding] class DataflowReactor(label: Int, val source: EventSource[Iterable[Int]]) extends (Iterable[Int] => Unit) {
    private val preserveSet = mutable.Set.empty[Int]
    def apply(newInfo: Iterable[Int]): Unit = {
        val oldSize = preserveSet.size
        preserveSet ++= newInfo
        if (preserveSet.size > oldSize) {
            source.fire(preserveSet)
        }
    }
    def preserve: Array[Int] = preserveSet.toSeq.sorted.toArray
}

private [deepembedding] sealed trait Event[+A]
private [deepembedding] case class Fired[A](x: A, handlers: Iterable[A => Unit]) extends Event[A]
private [deepembedding] case class Handled(f: () => Unit) extends Event[Nothing]

private [deepembedding] class EventCoordinator {
    val events = mutable.Queue.empty[Event[_]]
    def add(event: Event[_]): Unit = events.enqueue(event)
    def run(): Unit = {
        while (events.nonEmpty) events.dequeue() match {
            case Fired(x, handlers) => for (handler <- handlers) add(Handled(() => handler(x)))
            case Handled(f) => f()
        }
    }
}

private [deepembedding] abstract class EventStream[+A] {
    def foreach(f: A => Unit): Unit
}

private [deepembedding] class EventSource[A](implicit coordinator: EventCoordinator) extends EventStream[A] {
    val observers = mutable.Set.empty[A => Unit]
    def fire(x: A): Unit = coordinator.add(Fired(x, observers))
    def foreach(f: A => Unit): Unit = observers += f
}
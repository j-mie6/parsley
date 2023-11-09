package parsley.debugger.frontend

import org.scalatest.flatspec.AnyFlatSpec
import org.typelevel.scalaccompat.annotation.unused
import parsley.debugger.{DebugTree, ParseAttempt}

//noinspection ConvertExpressionToSAM
class StatefulnessSpec extends AnyFlatSpec {
    behavior of "the DebugFrontend class and its stateful / stateless enforcement"

    it should "throw when run multiple times, only if the frontend is stateful" in {
        // Dummy values.
        val stateless: StatelessFrontend = new StatelessFrontend {
            override protected def processImpl(input: => String, tree: => DebugTree): Unit = ()
        }

        val stateful: StatefulFrontend = new StatefulFrontend {
            override protected def processImpl(input: => String, tree: => DebugTree): Unit = ()
        }

        val tree: DebugTree = new DebugTree {
            override def parserName: String = "foo"

            override def internalName: String = parserName

            override def parseResults: Option[ParseAttempt] = None

            override def nodeChildren: Map[String, DebugTree] = Map.empty

            override def fullInput: String = "bar"
        }

        info("it should not throw when running a stateless frontend multiple times")
        stateless.process("bar", tree): @unused
        stateless.process("bar", tree): @unused

        // The first run should not throw.
        stateful.process("bar", tree): @unused
        info("it should throw when running a stateful frontend multiple times")
        try {
            stateful.process("bar", tree): @unused

            fail("stateful frontend did not throw an exception after running multiple times")
        } catch {
            case _: Throwable => info("stateful frontend has thrown after being run multiple times, as expected")
        }
    }
}

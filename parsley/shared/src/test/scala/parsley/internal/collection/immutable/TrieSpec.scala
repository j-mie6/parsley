package parsley.internal.collection.immutable

import org.scalatest.propspec.AnyPropSpec
import org.scalatest.matchers._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class TrieSpec extends AnyPropSpec with ScalaCheckPropertyChecks with should.Matchers {
    property("a Trie constructed from a set should contain all of its elements") {
        forAll { (set: List[String]) =>
            val t = Trie(set)
            set.forall(t.contains)
            for (key <- set) {
                t.contains(key) shouldBe true
            }
        }
    }

    property("a Trie constructed from a set should not contain extra keys") {
        forAll { (set: List[String]) =>
            val t = Trie(set)
            forAll { (str: String) =>
                whenever(!set.contains(str)) {
                    t.contains(str) shouldBe false
                }
            }
        }
    }
}

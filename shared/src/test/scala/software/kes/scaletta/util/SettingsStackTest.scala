package software.kes.scaletta.util

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class SettingsStackTest extends AnyFunSpec with Matchers {
  describe("SettingsStack") {
    it("should initialize with given settings") {
      val stack = SettingsStack.create(43)
      stack.current shouldBe 43
    }

    it("should support modify in place") {
      val stack = SettingsStack.create(1)
      stack.modify(_ + 1)
      stack.current shouldBe 2
    }

    it("should support push and pop") {
      val stack = SettingsStack.create(10)
      stack.push(_ + 5)
      stack.current shouldBe 15

      stack.pop()
      stack.current shouldBe 10
    }

    it("should support multiple nested pushes") {
      val stack = SettingsStack.create("a")
      stack.push(_ + "b")
      stack.current shouldBe "ab"
      stack.push(_ + "c")
      stack.current shouldBe "abc"

      stack.pop()
      stack.current shouldBe "ab"
      stack.pop()
      stack.current shouldBe "a"
    }

    it("should throw IllegalStateException when popping an empty stack") {
      val stack = SettingsStack.create(true)
      assertThrows[IllegalStateException] {
        stack.pop()
      }
    }

    it("should allow pushing the same settings multiple times") {
      val stack = SettingsStack.create(1)
      stack.push(identity) // push 1
      stack.push(identity) // push 1
      stack.current shouldBe 1

      stack.pop()
      stack.current shouldBe 1
      stack.pop()
      stack.current shouldBe 1
    }

    it("should allow modifying settings after push") {
      val stack = SettingsStack.create(100)
      stack.push(_ + 10) // current = 110, stack = [100]
      stack.modify(_ + 5) // current = 115, stack = [100]
      stack.current shouldBe 115

      stack.pop()
      stack.current shouldBe 100
    }
  }
}

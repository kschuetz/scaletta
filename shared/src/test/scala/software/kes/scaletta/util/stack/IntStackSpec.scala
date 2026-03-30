package software.kes.scaletta.util.stack

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class IntStackSpec extends AnyFunSpec with Matchers {
  describe("IntStack") {
    it("should be initially empty") {
      val stack = IntStack.create()
      stack.peek() shouldBe None
      stack.isEmpty shouldBe true
      stack.size() shouldBe 0
    }

    it("should push and peek values correctly") {
      val stack = IntStack.create()
      stack.push(41)
      stack.peek() shouldBe Some(41)
      stack.push(43)
      stack.peek() shouldBe Some(43)
    }

    it("should ensure peek does not change the state of the stack") {
      val stack = IntStack.create()
      stack.push(41)
      stack.push(43)

      stack.peek() shouldBe Some(43)
      stack.peek() shouldBe Some(43)

      stack.pop() shouldBe 43
      stack.peek() shouldBe Some(41)
      stack.peek() shouldBe Some(41)
    }

    it("should push and pop values correctly (LIFO)") {
      val stack = IntStack.create()
      stack.push(10)
      stack.push(20)
      stack.push(30)

      stack.pop() shouldBe 30
      stack.peek() shouldBe Some(20)

      stack.pop() shouldBe 20
      stack.peek() shouldBe Some(10)

      stack.pop() shouldBe 10
      stack.peek() shouldBe None
    }

    it("should handle a sequence of pushes and pops") {
      val stack = IntStack.create()
      val values = Seq(1, 2, 3, 4, 5, 6, 7)

      values.foreach(stack.push)

      values.reverse.foreach { expected =>
        stack.pop() shouldBe expected
      }

      stack.peek() shouldBe None
    }

    it("should support a large number of elements (testing growth)") {
      val stack = IntStack.create(initialCapacity = 2)
      val count = 1000
      for (i <- 1 to count) {
        stack.push(i)
      }

      stack.size() shouldBe count

      for (i <- count to 1 by -1) {
        stack.pop() shouldBe i
      }
      stack.peek() shouldBe None
      stack.isEmpty shouldBe true
    }

    it("should throw an exception when popping from an empty stack") {
      val stack = IntStack.create()
      assertThrows[NoSuchElementException] {
        stack.pop()
      }
    }

    it("should correctly report isEmpty and size") {
      val stack = IntStack.create()
      stack.isEmpty shouldBe true
      stack.size() shouldBe 0

      stack.push(41)
      stack.isEmpty shouldBe false
      stack.size() shouldBe 1

      stack.push(43)
      stack.size() shouldBe 2

      stack.pop()
      stack.size() shouldBe 1
      stack.isEmpty shouldBe false

      stack.pop()
      stack.isEmpty shouldBe true
      stack.size() shouldBe 0
    }

    it("should correctly clear the stack") {
      val stack = IntStack.create()
      stack.push(41)
      stack.push(43)
      stack.size() shouldBe 2
      stack.isEmpty shouldBe false

      stack.clear()
      stack.size() shouldBe 0
      stack.isEmpty shouldBe true
      stack.peek() shouldBe None
    }

    it("should support unsafeGet") {
      val stack = IntStack.create()
      stack.push(100) // index 2 from top
      stack.push(200) // index 1 from top
      stack.push(300) // index 0 from top

      stack.unsafeGet(0) shouldBe 300
      stack.unsafeGet(1) shouldBe 200
      stack.unsafeGet(2) shouldBe 100
    }
  }
}

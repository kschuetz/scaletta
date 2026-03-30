package software.kes.scaletta.util.stack

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ShortStackSpec extends AnyFunSpec with Matchers {
  describe("ShortStack") {
    it("should be initially empty") {
      val stack = ShortStack.create()
      stack.peek() shouldBe None
      stack.isEmpty shouldBe true
      stack.size() shouldBe 0
    }

    it("should push and peek values correctly") {
      val stack = ShortStack.create()
      stack.push(41.toShort)
      stack.peek() shouldBe Some(41.toShort)
      stack.push(43.toShort)
      stack.peek() shouldBe Some(43.toShort)
    }

    it("should ensure peek does not change the state of the stack") {
      val stack = ShortStack.create()
      stack.push(41.toShort)
      stack.push(43.toShort)

      stack.peek() shouldBe Some(43.toShort)
      stack.peek() shouldBe Some(43.toShort)

      stack.pop() shouldBe 43.toShort
      stack.peek() shouldBe Some(41.toShort)
      stack.peek() shouldBe Some(41.toShort)
    }

    it("should push and pop values correctly (LIFO)") {
      val stack = ShortStack.create()
      stack.push(10.toShort)
      stack.push(20.toShort)
      stack.push(30.toShort)

      stack.pop() shouldBe 30.toShort
      stack.peek() shouldBe Some(20.toShort)

      stack.pop() shouldBe 20.toShort
      stack.peek() shouldBe Some(10.toShort)

      stack.pop() shouldBe 10.toShort
      stack.peek() shouldBe None
    }

    it("should handle a sequence of pushes and pops") {
      val stack = ShortStack.create()
      val values = Seq(1, 2, 3, 4, 5, 6, 7).map(_.toShort)

      values.foreach(stack.push)

      values.reverse.foreach { expected =>
        stack.pop() shouldBe expected
      }

      stack.peek() shouldBe None
    }

    it("should support a large number of elements (testing growth)") {
      val stack = ShortStack.create(initialCapacity = 2)
      val count = 1000
      for (i <- 1 to count) {
        stack.push(i.toShort)
      }

      stack.size() shouldBe count

      for (i <- count to 1 by -1) {
        stack.pop() shouldBe i.toShort
      }
      stack.peek() shouldBe None
      stack.isEmpty shouldBe true
    }

    it("should throw an exception when popping from an empty stack") {
      val stack = ShortStack.create()
      assertThrows[NoSuchElementException] {
        stack.pop()
      }
    }

    it("should correctly report isEmpty and size") {
      val stack = ShortStack.create()
      stack.isEmpty shouldBe true
      stack.size() shouldBe 0

      stack.push(41.toShort)
      stack.isEmpty shouldBe false
      stack.size() shouldBe 1

      stack.push(43.toShort)
      stack.size() shouldBe 2

      stack.pop()
      stack.size() shouldBe 1
      stack.isEmpty shouldBe false

      stack.pop()
      stack.isEmpty shouldBe true
      stack.size() shouldBe 0
    }

    it("should correctly clear the stack") {
      val stack = ShortStack.create()
      stack.push(41.toShort)
      stack.push(43.toShort)
      stack.size() shouldBe 2
      stack.isEmpty shouldBe false

      stack.clear()
      stack.size() shouldBe 0
      stack.isEmpty shouldBe true
      stack.peek() shouldBe None
    }

    it("should support unsafeGet") {
      val stack = ShortStack.create()
      stack.push(100.toShort) // index 2 from top
      stack.push(200.toShort) // index 1 from top
      stack.push(300.toShort) // index 0 from top

      stack.unsafeGet(0) shouldBe 300.toShort
      stack.unsafeGet(1) shouldBe 200.toShort
      stack.unsafeGet(2) shouldBe 100.toShort
    }
  }
}

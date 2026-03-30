package software.kes.scaletta.util.stack

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ByteStackSpec extends AnyFunSpec with Matchers {
  describe("ByteStack") {
    it("should be initially empty") {
      val stack = ByteStack.create()
      stack.peek() shouldBe None
      stack.isEmpty shouldBe true
      stack.size() shouldBe 0
    }

    it("should push and peek values correctly") {
      val stack = ByteStack.create()
      stack.push(41.toByte)
      stack.peek() shouldBe Some(41.toByte)
      stack.push(43.toByte)
      stack.peek() shouldBe Some(43.toByte)
    }

    it("should ensure peek does not change the state of the stack") {
      val stack = ByteStack.create()
      stack.push(41.toByte)
      stack.push(43.toByte)

      stack.peek() shouldBe Some(43.toByte)
      stack.peek() shouldBe Some(43.toByte)

      stack.pop() shouldBe 43.toByte
      stack.peek() shouldBe Some(41.toByte)
      stack.peek() shouldBe Some(41.toByte)
    }

    it("should push and pop values correctly (LIFO)") {
      val stack = ByteStack.create()
      stack.push(10.toByte)
      stack.push(20.toByte)
      stack.push(30.toByte)

      stack.pop() shouldBe 30.toByte
      stack.peek() shouldBe Some(20.toByte)

      stack.pop() shouldBe 20.toByte
      stack.peek() shouldBe Some(10.toByte)

      stack.pop() shouldBe 10.toByte
      stack.peek() shouldBe None
    }

    it("should handle a sequence of pushes and pops") {
      val stack = ByteStack.create()
      val values = Seq(1, 2, 3, 4, 5, 6, 7).map(_.toByte)

      values.foreach(stack.push)

      values.reverse.foreach { expected =>
        stack.pop() shouldBe expected
      }

      stack.peek() shouldBe None
    }

    it("should support a large number of elements (testing growth)") {
      val stack = ByteStack.create(initialCapacity = 2)
      val count = 1000
      for (i <- 1 to count) {
        stack.push((i % 127).toByte)
      }

      stack.size() shouldBe count

      for (i <- count to 1 by -1) {
        stack.pop() shouldBe (i % 127).toByte
      }
      stack.peek() shouldBe None
      stack.isEmpty shouldBe true
    }

    it("should throw an exception when popping from an empty stack") {
      val stack = ByteStack.create()
      assertThrows[NoSuchElementException] {
        stack.pop()
      }
    }

    it("should correctly report isEmpty and size") {
      val stack = ByteStack.create()
      stack.isEmpty shouldBe true
      stack.size() shouldBe 0

      stack.push(41.toByte)
      stack.isEmpty shouldBe false
      stack.size() shouldBe 1

      stack.push(43.toByte)
      stack.size() shouldBe 2

      stack.pop()
      stack.size() shouldBe 1
      stack.isEmpty shouldBe false

      stack.pop()
      stack.isEmpty shouldBe true
      stack.size() shouldBe 0
    }

    it("should correctly clear the stack") {
      val stack = ByteStack.create()
      stack.push(41.toByte)
      stack.push(43.toByte)
      stack.size() shouldBe 2
      stack.isEmpty shouldBe false

      stack.clear()
      stack.size() shouldBe 0
      stack.isEmpty shouldBe true
      stack.peek() shouldBe None
    }

    it("should support unsafeGet") {
      val stack = ByteStack.create()
      stack.push(100.toByte) // index 2 from top
      stack.push(110.toByte) // index 1 from top
      stack.push(120.toByte) // index 0 from top

      stack.unsafeGet(0) shouldBe 120.toByte
      stack.unsafeGet(1) shouldBe 110.toByte
      stack.unsafeGet(2) shouldBe 100.toByte
    }
  }
}

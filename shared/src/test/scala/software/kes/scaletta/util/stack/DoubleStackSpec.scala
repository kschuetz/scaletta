package software.kes.scaletta.util.stack

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class DoubleStackSpec extends AnyFunSpec with Matchers {
  describe("DoubleStack") {
    it("should be initially empty") {
      val stack = DoubleStack.create()
      stack.peek() shouldBe None
      stack.isEmpty shouldBe true
      stack.size() shouldBe 0
    }

    it("should push and peek values correctly") {
      val stack = DoubleStack.create()
      stack.push(41.0)
      stack.peek() shouldBe Some(41.0)
      stack.push(43.0)
      stack.peek() shouldBe Some(43.0)
    }

    it("should ensure peek does not change the state of the stack") {
      val stack = DoubleStack.create()
      stack.push(41.0)
      stack.push(43.0)

      stack.peek() shouldBe Some(43.0)
      stack.peek() shouldBe Some(43.0)

      stack.pop() shouldBe 43.0
      stack.peek() shouldBe Some(41.0)
      stack.peek() shouldBe Some(41.0)
    }

    it("should push and pop values correctly (LIFO)") {
      val stack = DoubleStack.create()
      stack.push(10.5)
      stack.push(20.5)
      stack.push(30.5)

      stack.pop() shouldBe 30.5
      stack.peek() shouldBe Some(20.5)

      stack.pop() shouldBe 20.5
      stack.peek() shouldBe Some(10.5)

      stack.pop() shouldBe 10.5
      stack.peek() shouldBe None
    }

    it("should handle a sequence of pushes and pops") {
      val stack = DoubleStack.create()
      val values = Seq(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7)

      values.foreach(stack.push)

      values.reverse.foreach { expected =>
        stack.pop() shouldBe expected
      }

      stack.peek() shouldBe None
    }

    it("should support a large number of elements (testing growth)") {
      val stack = DoubleStack.create(initialCapacity = 2)
      val count = 1000
      for (i <- 1 to count) {
        stack.push(i.toDouble)
      }

      stack.size() shouldBe count

      for (i <- count to 1 by -1) {
        stack.pop() shouldBe i.toDouble
      }
      stack.peek() shouldBe None
      stack.isEmpty shouldBe true
    }

    it("should throw an exception when popping from an empty stack") {
      val stack = DoubleStack.create()
      assertThrows[NoSuchElementException] {
        stack.pop()
      }
    }

    it("should correctly report isEmpty and size") {
      val stack = DoubleStack.create()
      stack.isEmpty shouldBe true
      stack.size() shouldBe 0

      stack.push(41.0)
      stack.isEmpty shouldBe false
      stack.size() shouldBe 1

      stack.push(43.0)
      stack.size() shouldBe 2

      stack.pop()
      stack.size() shouldBe 1
      stack.isEmpty shouldBe false

      stack.pop()
      stack.isEmpty shouldBe true
      stack.size() shouldBe 0
    }

    it("should correctly clear the stack") {
      val stack = DoubleStack.create()
      stack.push(41.0)
      stack.push(43.0)
      stack.size() shouldBe 2
      stack.isEmpty shouldBe false

      stack.clear()
      stack.size() shouldBe 0
      stack.isEmpty shouldBe true
      stack.peek() shouldBe None
    }

    it("should support unsafeRead") {
      val stack = DoubleStack.create()
      stack.push(100.1) // index 2 from top
      stack.push(200.2) // index 1 from top
      stack.push(300.3) // index 0 from top

      stack.unsafeRead(0) shouldBe 300.3
      stack.unsafeRead(1) shouldBe 200.2
      stack.unsafeRead(2) shouldBe 100.1
    }

    it("should support unsafeWrite") {
      val stack = DoubleStack.create()
      stack.push(100.1)
      stack.push(200.2)
      stack.push(300.3)

      stack.unsafeWrite(0, 41.1)
      stack.unsafeRead(0) shouldBe 41.1

      stack.unsafeWrite(1, 43.3)
      stack.unsafeRead(1) shouldBe 43.3

      stack.unsafeWrite(2, 45.5)
      stack.unsafeRead(2) shouldBe 45.5
    }
  }
}

package software.kes.scaletta.util.stack

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class FloatStackSpec extends AnyFunSpec with Matchers {
  describe("FloatStack") {
    it("should be initially empty") {
      val stack = FloatStack.create()
      stack.peek() shouldBe None
      stack.isEmpty shouldBe true
      stack.size() shouldBe 0
    }

    it("should push and peek values correctly") {
      val stack = FloatStack.create()
      stack.push(41.0f)
      stack.peek() shouldBe Some(41.0f)
      stack.push(43.0f)
      stack.peek() shouldBe Some(43.0f)
    }

    it("should ensure peek does not change the state of the stack") {
      val stack = FloatStack.create()
      stack.push(41.0f)
      stack.push(43.0f)

      stack.peek() shouldBe Some(43.0f)
      stack.peek() shouldBe Some(43.0f)

      stack.pop() shouldBe 43.0f
      stack.peek() shouldBe Some(41.0f)
      stack.peek() shouldBe Some(41.0f)
    }

    it("should push and pop values correctly (LIFO)") {
      val stack = FloatStack.create()
      stack.push(10.5f)
      stack.push(20.5f)
      stack.push(30.5f)

      stack.pop() shouldBe 30.5f
      stack.peek() shouldBe Some(20.5f)

      stack.pop() shouldBe 20.5f
      stack.peek() shouldBe Some(10.5f)

      stack.pop() shouldBe 10.5f
      stack.peek() shouldBe None
    }

    it("should handle a sequence of pushes and pops") {
      val stack = FloatStack.create()
      val values = Seq(1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f, 7.7f)

      values.foreach(stack.push)

      values.reverse.foreach { expected =>
        stack.pop() shouldBe expected
      }

      stack.peek() shouldBe None
    }

    it("should support a large number of elements (testing growth)") {
      val stack = FloatStack.create(initialCapacity = 2)
      val count = 1000
      for (i <- 1 to count) {
        stack.push(i.toFloat)
      }

      stack.size() shouldBe count

      for (i <- count to 1 by -1) {
        stack.pop() shouldBe i.toFloat
      }
      stack.peek() shouldBe None
      stack.isEmpty shouldBe true
    }

    it("should throw an exception when popping from an empty stack") {
      val stack = FloatStack.create()
      assertThrows[NoSuchElementException] {
        stack.pop()
      }
    }

    it("should correctly report isEmpty and size") {
      val stack = FloatStack.create()
      stack.isEmpty shouldBe true
      stack.size() shouldBe 0

      stack.push(41.0f)
      stack.isEmpty shouldBe false
      stack.size() shouldBe 1

      stack.push(43.0f)
      stack.size() shouldBe 2

      stack.pop()
      stack.size() shouldBe 1
      stack.isEmpty shouldBe false

      stack.pop()
      stack.isEmpty shouldBe true
      stack.size() shouldBe 0
    }

    it("should correctly clear the stack") {
      val stack = FloatStack.create()
      stack.push(41.0f)
      stack.push(43.0f)
      stack.size() shouldBe 2
      stack.isEmpty shouldBe false

      stack.clear()
      stack.size() shouldBe 0
      stack.isEmpty shouldBe true
      stack.peek() shouldBe None
    }

    it("should support unsafeGet") {
      val stack = FloatStack.create()
      stack.push(100.1f) // index 2 from top
      stack.push(200.2f) // index 1 from top
      stack.push(300.3f) // index 0 from top

      stack.unsafeGet(0) shouldBe 300.3f
      stack.unsafeGet(1) shouldBe 200.2f
      stack.unsafeGet(2) shouldBe 100.1f
    }
  }
}

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

    it("should support unsafeRead") {
      val stack = FloatStack.create()
      stack.push(100.1f) // index 2 from top
      stack.push(200.2f) // index 1 from top
      stack.push(300.3f) // index 0 from top

      stack.unsafeRead(0) shouldBe 300.3f
      stack.unsafeRead(1) shouldBe 200.2f
      stack.unsafeRead(2) shouldBe 100.1f
    }

    it("should support unsafeWrite") {
      val stack = FloatStack.create()
      stack.push(100.1f)
      stack.push(200.2f)
      stack.push(300.3f)

      stack.unsafeWrite(0, 41.1f)
      stack.unsafeRead(0) shouldBe 41.1f

      stack.unsafeWrite(1, 43.3f)
      stack.unsafeRead(1) shouldBe 43.3f

      stack.unsafeWrite(2, 45.5f)
      stack.unsafeRead(2) shouldBe 45.5f
    }

    it("should support expand and contract") {
      val stack = FloatStack.create(initialCapacity = 2)
      stack.push(41.1f)
      stack.push(43.3f)

      stack.expand(3)
      stack.size() shouldBe 5

      stack.unsafeWrite(0, 49.9f)
      stack.unsafeWrite(1, 47.7f)
      stack.unsafeWrite(2, 45.5f)

      stack.peek() shouldBe Some(49.9f)
      stack.pop() shouldBe 49.9f
      stack.pop() shouldBe 47.7f
      stack.pop() shouldBe 45.5f
      stack.pop() shouldBe 43.3f
      stack.pop() shouldBe 41.1f
      stack.isEmpty shouldBe true

      stack.push(41.1f)
      stack.push(43.3f)
      stack.push(45.5f)
      stack.contract(2)
      stack.size() shouldBe 1
      stack.peek() shouldBe Some(41.1f)
      stack.pop() shouldBe 41.1f
      stack.isEmpty shouldBe true
    }

    it("should handle invalid expand and contract amounts") {
      val stack = FloatStack.create()
      stack.push(41.1f)
      stack.expand(0)
      stack.size() shouldBe 1
      stack.expand(-43)
      stack.size() shouldBe 1

      stack.contract(0)
      stack.size() shouldBe 1
      stack.contract(-43)
      stack.size() shouldBe 1

      stack.contract(100)
      stack.size() shouldBe 0
    }

    it("should duplicate the top value") {
      val stack = FloatStack.create()
      stack.push(41.1f)
      stack.duplicate()
      stack.size() shouldBe 2
      stack.pop() shouldBe 41.1f
      stack.pop() shouldBe 41.1f
      stack.isEmpty shouldBe true

      stack.push(43.3f)
      stack.push(47.7f)
      stack.duplicate()
      stack.size() shouldBe 3
      stack.pop() shouldBe 47.7f
      stack.pop() shouldBe 47.7f
      stack.pop() shouldBe 43.3f
    }

    it("should throw NoSuchElementException when duplicating an empty stack") {
      val stack = FloatStack.create()
      assertThrows[NoSuchElementException] {
        stack.duplicate()
      }
    }

    it("should swap top two elements") {
      val stack = FloatStack.create()
      stack.push(41.0f)
      stack.push(43.0f)
      stack.swap()
      stack.pop() shouldBe 41.0f
      stack.pop() shouldBe 43.0f
    }

    it("should do nothing on swap if stack has 1 element") {
      val stack = FloatStack.create()
      stack.push(41.0f)
      stack.swap()
      stack.size() shouldBe 1
      stack.peek() shouldBe Some(41.0f)
    }

    it("should do nothing on swap if stack is empty") {
      val stack = FloatStack.create()
      stack.swap()
      stack.size() shouldBe 0
      stack.isEmpty shouldBe true
    }
  }
}

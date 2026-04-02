package software.kes.scaletta.util.stack

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class LongStackSpec extends AnyFunSpec with Matchers {
  describe("LongStack") {
    it("should be initially empty") {
      val stack = LongStack.create()
      stack.peek() shouldBe None
      stack.isEmpty shouldBe true
      stack.size() shouldBe 0
    }

    it("should push and peek values correctly") {
      val stack = LongStack.create()
      stack.push(41L)
      stack.peek() shouldBe Some(41L)
      stack.push(43L)
      stack.peek() shouldBe Some(43L)
    }

    it("should ensure peek does not change the state of the stack") {
      val stack = LongStack.create()
      stack.push(41L)
      stack.push(43L)

      stack.peek() shouldBe Some(43L)
      stack.peek() shouldBe Some(43L)

      stack.pop() shouldBe 43L
      stack.peek() shouldBe Some(41L)
      stack.peek() shouldBe Some(41L)
    }

    it("should push and pop values correctly (LIFO)") {
      val stack = LongStack.create()
      stack.push(10L)
      stack.push(20L)
      stack.push(30L)

      stack.pop() shouldBe 30L
      stack.peek() shouldBe Some(20L)

      stack.pop() shouldBe 20L
      stack.peek() shouldBe Some(10L)

      stack.pop() shouldBe 10L
      stack.peek() shouldBe None
    }

    it("should handle a sequence of pushes and pops") {
      val stack = LongStack.create()
      val values = Seq(1L, 2L, 3L, 4L, 5L, 6L, 7L)

      values.foreach(stack.push)

      values.reverse.foreach { expected =>
        stack.pop() shouldBe expected
      }

      stack.peek() shouldBe None
    }

    it("should support a large number of elements (testing growth)") {
      val stack = LongStack.create(initialCapacity = 2)
      val count = 1000
      for (i <- 1L to count.toLong) {
        stack.push(i)
      }

      stack.size() shouldBe count

      for (i <- count.toLong to 1L by -1L) {
        stack.pop() shouldBe i
      }
      stack.peek() shouldBe None
      stack.isEmpty shouldBe true
    }

    it("should throw an exception when popping from an empty stack") {
      val stack = LongStack.create()
      assertThrows[NoSuchElementException] {
        stack.pop()
      }
    }

    it("should correctly report isEmpty and size") {
      val stack = LongStack.create()
      stack.isEmpty shouldBe true
      stack.size() shouldBe 0

      stack.push(41L)
      stack.isEmpty shouldBe false
      stack.size() shouldBe 1

      stack.push(43L)
      stack.size() shouldBe 2

      stack.pop()
      stack.size() shouldBe 1
      stack.isEmpty shouldBe false

      stack.pop()
      stack.isEmpty shouldBe true
      stack.size() shouldBe 0
    }

    it("should correctly clear the stack") {
      val stack = LongStack.create()
      stack.push(41L)
      stack.push(43L)
      stack.size() shouldBe 2
      stack.isEmpty shouldBe false

      stack.clear()
      stack.size() shouldBe 0
      stack.isEmpty shouldBe true
      stack.peek() shouldBe None
    }

    it("should support unsafeRead") {
      val stack = LongStack.create()
      stack.push(100L) // index 2 from top
      stack.push(200L) // index 1 from top
      stack.push(300L) // index 0 from top

      stack.unsafeRead(0) shouldBe 300L
      stack.unsafeRead(1) shouldBe 200L
      stack.unsafeRead(2) shouldBe 100L
    }

    it("should support unsafeWrite") {
      val stack = LongStack.create()
      stack.push(100L)
      stack.push(200L)
      stack.push(300L)

      stack.unsafeWrite(0, 41L)
      stack.unsafeRead(0) shouldBe 41L

      stack.unsafeWrite(1, 43L)
      stack.unsafeRead(1) shouldBe 43L

      stack.unsafeWrite(2, 45L)
      stack.unsafeRead(2) shouldBe 45L
    }

    it("should support expand and contract") {
      val stack = LongStack.create(initialCapacity = 2)
      stack.push(41L)
      stack.push(43L)

      stack.expand(3)
      stack.size() shouldBe 5

      stack.unsafeWrite(0, 49L)
      stack.unsafeWrite(1, 47L)
      stack.unsafeWrite(2, 45L)

      stack.peek() shouldBe Some(49L)
      stack.pop() shouldBe 49L
      stack.pop() shouldBe 47L
      stack.pop() shouldBe 45L
      stack.pop() shouldBe 43L
      stack.pop() shouldBe 41L
      stack.isEmpty shouldBe true

      stack.push(41L)
      stack.push(43L)
      stack.push(45L)
      stack.contract(2)
      stack.size() shouldBe 1
      stack.peek() shouldBe Some(41L)
      stack.pop() shouldBe 41L
      stack.isEmpty shouldBe true
    }

    it("should handle invalid expand and contract amounts") {
      val stack = LongStack.create()
      stack.push(41L)
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
  }
}

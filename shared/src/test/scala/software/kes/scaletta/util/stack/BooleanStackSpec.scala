package software.kes.scaletta.util.stack

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

//noinspection NameBooleanParameters
class BooleanStackSpec extends AnyFunSpec with Matchers {
  describe("BooleanStack") {
    it("should be initially empty") {
      val stack = BooleanStack.create()
      stack.peek() shouldBe None
    }

    it("should push and peek values correctly") {
      val stack = BooleanStack.create()
      stack.push(true)
      stack.peek() shouldBe Some(true)
      stack.push(false)
      stack.peek() shouldBe Some(false)
    }

    it("should ensure peek does not change the state of the stack") {
      val stack = BooleanStack.create()
      stack.push(true)
      stack.push(false)

      stack.peek() shouldBe Some(false)
      stack.peek() shouldBe Some(false) // Repeated peek should yield same result

      stack.pop() shouldBe false
      stack.peek() shouldBe Some(true)
      stack.peek() shouldBe Some(true) // Repeated peek after pop should yield same result
    }

    it("should push and pop values correctly (LIFO)") {
      val stack = BooleanStack.create()
      stack.push(true)
      stack.push(false)
      stack.push(true)

      stack.pop() shouldBe true
      stack.peek() shouldBe Some(false)

      stack.pop() shouldBe false
      stack.peek() shouldBe Some(true)

      stack.pop() shouldBe true
      stack.peek() shouldBe None
    }

    it("should handle a sequence of pushes and pops") {
      val stack = BooleanStack.create()
      val values = Seq(true, false, false, true, true, false, true)

      values.foreach(stack.push)

      values.reverse.foreach { expected =>
        stack.pop() shouldBe expected
      }

      stack.peek() shouldBe None
    }

    it("should support a large number of elements (testing growth)") {
      val stack = BooleanStack.create()
      val count = 1000
      for (i <- 1 to count) {
        stack.push(i % 2 == 0)
      }

      for (i <- count to 1 by -1) {
        stack.pop() shouldBe (i % 2 == 0)
      }
      stack.peek() shouldBe None
    }

    it("should throw an exception when popping from an empty stack") {
      val stack = BooleanStack.create()
      assertThrows[NoSuchElementException] {
        stack.pop()
      }
    }

    it("should correctly report isEmpty and size") {
      val stack = BooleanStack.create()
      stack.isEmpty shouldBe true
      stack.size() shouldBe 0

      stack.push(true)
      stack.isEmpty shouldBe false
      stack.size() shouldBe 1

      stack.push(false)
      stack.size() shouldBe 2

      stack.pop()
      stack.size() shouldBe 1
      stack.isEmpty shouldBe false

      stack.pop()
      stack.isEmpty shouldBe true
      stack.size() shouldBe 0
    }

    it("should correctly handle crossing 64-bit boundaries") {
      val stack = BooleanStack.create()
      for (i <- 1 to 100) {
        stack.push(i % 3 == 0)
      }

      stack.size() shouldBe 100

      // Verify values around the 64-bit boundary (index 63/64)
      // We'll pop them all and check
      for (i <- 100 to 1 by -1) {
        stack.pop() shouldBe (i % 3 == 0)
      }
    }

    it("should correctly clear the stack") {
      val stack = BooleanStack.create()
      stack.push(true)
      stack.push(false)
      stack.size() shouldBe 2
      stack.isEmpty shouldBe false

      stack.clear()
      stack.size() shouldBe 0
      stack.isEmpty shouldBe true
      stack.peek() shouldBe None
    }

    it("should support unsafeRead") {
      val stack = BooleanStack.create()
      stack.push(true) // pos 2
      stack.push(false) // pos 1
      stack.push(true) // pos 0

      stack.unsafeRead(0) shouldBe true
      stack.unsafeRead(1) shouldBe false
      stack.unsafeRead(2) shouldBe true
    }

    it("should support unsafeWrite") {
      val stack = BooleanStack.create()
      stack.push(true)
      stack.push(false)
      stack.push(true)

      stack.unsafeWrite(0, false)
      stack.unsafeRead(0) shouldBe false

      stack.unsafeWrite(1, true)
      stack.unsafeRead(1) shouldBe true

      stack.unsafeWrite(2, false)
      stack.unsafeRead(2) shouldBe false
    }

    it("should support expand and contract") {
      val stack = BooleanStack.create()
      stack.push(true)
      stack.push(false)

      stack.expand(3)
      stack.size() shouldBe 5

      stack.unsafeWrite(0, true)
      stack.unsafeWrite(1, false)
      stack.unsafeWrite(2, true)

      stack.peek() shouldBe Some(true)
      stack.pop() shouldBe true
      stack.pop() shouldBe false
      stack.pop() shouldBe true
      stack.pop() shouldBe false
      stack.pop() shouldBe true
      stack.isEmpty shouldBe true

      stack.push(true)
      stack.push(false)
      stack.push(true)
      stack.contract(2)
      stack.size() shouldBe 1
      stack.peek() shouldBe Some(true)
      stack.pop() shouldBe true
      stack.isEmpty shouldBe true
    }

    it("should handle invalid expand and contract amounts") {
      val stack = BooleanStack.create()
      stack.push(true)
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
      val stack = BooleanStack.create()
      stack.push(true)
      stack.duplicate()
      stack.size() shouldBe 2
      stack.pop() shouldBe true
      stack.pop() shouldBe true
      stack.isEmpty shouldBe true

      stack.push(false)
      stack.push(true)
      stack.duplicate()
      stack.size() shouldBe 3
      stack.pop() shouldBe true
      stack.pop() shouldBe true
      stack.pop() shouldBe false
    }

    it("should throw NoSuchElementException when duplicating an empty stack") {
      val stack = BooleanStack.create()
      assertThrows[NoSuchElementException] {
        stack.duplicate()
      }
    }

    it("should swap top two elements") {
      val stack = BooleanStack.create()
      stack.push(true)
      stack.push(false)
      stack.swap()
      stack.pop() shouldBe true
      stack.pop() shouldBe false
    }

    it("should do nothing on swap if stack has 1 element") {
      val stack = BooleanStack.create()
      stack.push(true)
      stack.swap()
      stack.size() shouldBe 1
      stack.peek() shouldBe Some(true)
    }

    it("should do nothing on swap if stack is empty") {
      val stack = BooleanStack.create()
      stack.swap()
      stack.size() shouldBe 0
      stack.isEmpty shouldBe true
    }
  }
}

package software.kes.scaletta.util.stack

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class CharStackSpec extends AnyFunSpec with Matchers {
  describe("CharStack") {
    it("should be initially empty") {
      val stack = CharStack.create()
      stack.peek() shouldBe None
      stack.isEmpty shouldBe true
      stack.size() shouldBe 0
    }

    it("should push and peek values correctly") {
      val stack = CharStack.create()
      stack.push('A')
      stack.peek() shouldBe Some('A')
      stack.push('B')
      stack.peek() shouldBe Some('B')
    }

    it("should ensure peek does not change the state of the stack") {
      val stack = CharStack.create()
      stack.push('A')
      stack.push('B')

      stack.peek() shouldBe Some('B')
      stack.peek() shouldBe Some('B')

      stack.pop() shouldBe 'B'
      stack.peek() shouldBe Some('A')
      stack.peek() shouldBe Some('A')
    }

    it("should push and pop values correctly (LIFO)") {
      val stack = CharStack.create()
      stack.push('x')
      stack.push('y')
      stack.push('z')

      stack.pop() shouldBe 'z'
      stack.peek() shouldBe Some('y')

      stack.pop() shouldBe 'y'
      stack.peek() shouldBe Some('x')

      stack.pop() shouldBe 'x'
      stack.peek() shouldBe None
    }

    it("should handle a sequence of pushes and pops") {
      val stack = CharStack.create()
      val values = Seq('a', 'b', 'c', 'd', 'e', 'f', 'g')

      values.foreach(stack.push)

      values.reverse.foreach { expected =>
        stack.pop() shouldBe expected
      }

      stack.peek() shouldBe None
    }

    it("should support a large number of elements (testing growth)") {
      val stack = CharStack.create(initialCapacity = 2)
      val count = 1000
      for (i <- 1 to count) {
        stack.push(i.toChar)
      }

      stack.size() shouldBe count

      for (i <- count to 1 by -1) {
        stack.pop() shouldBe i.toChar
      }
      stack.peek() shouldBe None
      stack.isEmpty shouldBe true
    }

    it("should throw an exception when popping from an empty stack") {
      val stack = CharStack.create()
      assertThrows[NoSuchElementException] {
        stack.pop()
      }
    }

    it("should correctly report isEmpty and size") {
      val stack = CharStack.create()
      stack.isEmpty shouldBe true
      stack.size() shouldBe 0

      stack.push('1')
      stack.isEmpty shouldBe false
      stack.size() shouldBe 1

      stack.push('2')
      stack.size() shouldBe 2

      stack.pop()
      stack.size() shouldBe 1
      stack.isEmpty shouldBe false

      stack.pop()
      stack.isEmpty shouldBe true
      stack.size() shouldBe 0
    }

    it("should correctly clear the stack") {
      val stack = CharStack.create()
      stack.push('a')
      stack.push('b')
      stack.size() shouldBe 2
      stack.isEmpty shouldBe false

      stack.clear()
      stack.size() shouldBe 0
      stack.isEmpty shouldBe true
      stack.peek() shouldBe None
    }

    it("should support unsafeRead") {
      val stack = CharStack.create()
      stack.push('c') // index 2 from top
      stack.push('b') // index 1 from top
      stack.push('a') // index 0 from top

      stack.unsafeRead(0) shouldBe 'a'
      stack.unsafeRead(1) shouldBe 'b'
      stack.unsafeRead(2) shouldBe 'c'
    }

    it("should support unsafeWrite") {
      val stack = CharStack.create()
      stack.push('c')
      stack.push('b')
      stack.push('a')

      stack.unsafeWrite(0, 'x')
      stack.unsafeRead(0) shouldBe 'x'

      stack.unsafeWrite(1, 'y')
      stack.unsafeRead(1) shouldBe 'y'

      stack.unsafeWrite(2, 'z')
      stack.unsafeRead(2) shouldBe 'z'
    }
  }
}

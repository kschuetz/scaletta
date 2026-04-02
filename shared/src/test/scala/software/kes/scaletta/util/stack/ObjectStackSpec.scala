package software.kes.scaletta.util.stack

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ObjectStackSpec extends AnyFunSpec with Matchers {
  describe("ObjectStack") {
    it("should be initially empty") {
      val stack = ObjectStack.create()
      stack.peek() shouldBe None
      stack.isEmpty shouldBe true
      stack.size() shouldBe 0
    }

    it("should push and peek values correctly") {
      val stack = ObjectStack.create()
      val v1 = "41"
      val v2 = "43"
      stack.push(v1)
      stack.peek() shouldBe Some(v1)
      stack.push(v2)
      stack.peek() shouldBe Some(v2)
    }

    it("should ensure peek does not change the state of the stack") {
      val stack = ObjectStack.create()
      val v1 = "41"
      val v2 = "43"
      stack.push(v1)
      stack.push(v2)

      stack.peek() shouldBe Some(v2)
      stack.peek() shouldBe Some(v2)

      stack.pop() shouldBe v2
      stack.peek() shouldBe Some(v1)
      stack.peek() shouldBe Some(v1)
    }

    it("should push and pop values correctly (LIFO)") {
      val stack = ObjectStack.create()
      val v1 = "10.5"
      val v2 = "20.5"
      val v3 = "30.5"
      stack.push(v1)
      stack.push(v2)
      stack.push(v3)

      stack.pop() shouldBe v3
      stack.peek() shouldBe Some(v2)

      stack.pop() shouldBe v2
      stack.peek() shouldBe Some(v1)

      stack.pop() shouldBe v1
      stack.peek() shouldBe None
    }

    it("should handle a sequence of pushes and pops") {
      val stack = ObjectStack.create()
      val values = Seq("1.1", "2.2", "3.3", "4.4", "5.5", "6.6", "7.7")

      values.foreach(stack.push)

      values.reverse.foreach { expected =>
        stack.pop() shouldBe expected
      }

      stack.peek() shouldBe None
    }

    it("should support a large number of elements (testing growth)") {
      val stack = ObjectStack.create(initialCapacity = 2)
      val count = 1000
      for (i <- 1 to count) {
        stack.push(i.toString)
      }

      stack.size() shouldBe count

      for (i <- count to 1 by -1) {
        stack.pop() shouldBe i.toString
      }
      stack.peek() shouldBe None
      stack.isEmpty shouldBe true
    }

    it("should throw an exception when popping from an empty stack") {
      val stack = ObjectStack.create()
      assertThrows[NoSuchElementException] {
        stack.pop()
      }
    }

    it("should correctly report isEmpty and size") {
      val stack = ObjectStack.create()
      stack.isEmpty shouldBe true
      stack.size() shouldBe 0

      stack.push("41")
      stack.isEmpty shouldBe false
      stack.size() shouldBe 1

      stack.push("43")
      stack.size() shouldBe 2

      stack.pop()
      stack.size() shouldBe 1
      stack.isEmpty shouldBe false

      stack.pop()
      stack.isEmpty shouldBe true
      stack.size() shouldBe 0
    }

    it("should correctly clear the stack") {
      val stack = ObjectStack.create()
      stack.push("41")
      stack.push("43")
      stack.size() shouldBe 2
      stack.isEmpty shouldBe false

      stack.clear()
      stack.size() shouldBe 0
      stack.isEmpty shouldBe true
      stack.peek() shouldBe None
    }

    it("should support unsafeRead") {
      val stack = ObjectStack.create()
      val v1 = "100.1"
      val v2 = "200.2"
      val v3 = "300.3"
      stack.push(v1) // index 2 from top
      stack.push(v2) // index 1 from top
      stack.push(v3) // index 0 from top

      stack.unsafeRead(0) shouldBe v3
      stack.unsafeRead(1) shouldBe v2
      stack.unsafeRead(2) shouldBe v1
    }

    it("should support unsafeWrite") {
      val stack = ObjectStack.create()
      val v1 = "100.1"
      val v2 = "200.2"
      val v3 = "300.3"
      stack.push(v1)
      stack.push(v2)
      stack.push(v3)

      val n1 = "41"
      val n2 = "43"
      val n3 = "45"

      stack.unsafeWrite(0, n1)
      stack.unsafeRead(0) shouldBe n1

      stack.unsafeWrite(1, n2)
      stack.unsafeRead(1) shouldBe n2

      stack.unsafeWrite(2, n3)
      stack.unsafeRead(2) shouldBe n3
    }
  }
}

package software.kes.scaletta.interpreter

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.common.BasicTypes

class OperandStackSpec extends AnyFunSpec with Matchers {
  describe("OperandStack") {
    it("should be initially empty") {
      val stack = OperandStack.create()
      stack.isEmpty shouldBe true
      stack.size() shouldBe 0
      stack.peek shouldBe None
      stack.peekBasicType shouldBe None
    }

    it("should push and pop objects correctly") {
      val stack = OperandStack.create()
      val v1 = "41"
      val v2 = "43"
      stack.pushObject(v1)
      stack.size() shouldBe 1
      stack.peek shouldBe Some(v1)
      stack.peekBasicType shouldBe Some(BasicTypes.Object)

      stack.pushObject(v2)
      stack.size() shouldBe 2
      stack.peek shouldBe Some(v2)

      stack.pop() shouldBe v2
      stack.pop() shouldBe v1
      stack.isEmpty shouldBe true
    }

    it("should push and pop primitives correctly") {
      val stack = OperandStack.create()

      stack.pushBoolean(true)
      stack.peek shouldBe Some(true)
      stack.peekBasicType shouldBe Some(BasicTypes.Boolean)
      stack.pop() shouldBe true

      stack.pushInt(41)
      stack.peek shouldBe Some(41)
      stack.peekBasicType shouldBe Some(BasicTypes.Int)
      stack.pop() shouldBe 41

      stack.pushLong(43L)
      stack.peek shouldBe Some(43L)
      stack.peekBasicType shouldBe Some(BasicTypes.Long)
      stack.pop() shouldBe 43L

      stack.pushShort(41.toShort)
      stack.peek shouldBe Some(41.toShort)
      stack.peekBasicType shouldBe Some(BasicTypes.Short)
      stack.pop() shouldBe 41.toShort

      stack.pushByte(43.toByte)
      stack.peek shouldBe Some(43.toByte)
      stack.peekBasicType shouldBe Some(BasicTypes.Byte)
      stack.pop() shouldBe 43.toByte

      stack.pushChar('A')
      stack.peek shouldBe Some('A')
      stack.peekBasicType shouldBe Some(BasicTypes.Char)
      stack.pop() shouldBe 'A'

      stack.pushDouble(41.5)
      stack.peek shouldBe Some(41.5)
      stack.peekBasicType shouldBe Some(BasicTypes.Double)
      stack.pop() shouldBe 41.5

      stack.pushFloat(43.5f)
      stack.peek shouldBe Some(43.5f)
      stack.peekBasicType shouldBe Some(BasicTypes.Float)
      stack.pop() shouldBe 43.5f
    }

    it("should handle polymorphic push and pop") {
      val stack = OperandStack.create()
      stack.push(41)
      stack.push("43")
      stack.push(true)

      stack.pop() shouldBe true
      stack.pop() shouldBe "43"
      stack.pop() shouldBe 41
    }

    it("should support unsafePop variants") {
      val stack = OperandStack.create()
      stack.pushInt(41)
      stack.pushObject("43")
      stack.pushBoolean(false)

      stack.unsafePopBoolean() shouldBe false
      stack.unsafePopObject() shouldBe "43"
      stack.unsafePopInt() shouldBe 41
      stack.isEmpty shouldBe true
    }

    it("should maintain LIFO order across different types") {
      val stack = OperandStack.create()
      stack.pushInt(1)
      stack.pushObject("two")
      stack.pushDouble(3.0)

      stack.size() shouldBe 3
      stack.pop() shouldBe 3.0
      stack.pop() shouldBe "two"
      stack.pop() shouldBe 1
      stack.isEmpty shouldBe true
    }

    it("should correctly clear all internal stacks") {
      val stack = OperandStack.create()
      stack.pushInt(41)
      stack.pushObject("43")
      stack.clear()

      stack.isEmpty shouldBe true
      stack.size() shouldBe 0
      stack.peek shouldBe None
    }

    it("should throw exception when popping from empty stack") {
      val stack = OperandStack.create()
      assertThrows[NoSuchElementException] {
        stack.pop()
      }
    }

    it("should correctly handle many elements of different types") {
      val stack = OperandStack.create()
      val count = 100
      for (i <- 1 to count) {
        if (i % 2 == 0) stack.pushInt(i)
        else stack.pushObject(i.toString)
      }

      stack.size() shouldBe count

      for (i <- count to 1 by -1) {
        if (i % 2 == 0) stack.pop() shouldBe i
        else stack.pop() shouldBe i.toString
      }

      stack.isEmpty shouldBe true
    }

    it("should correctly evaluate conditions with popCondition") {
      val stack = OperandStack.create()

      // Objects
      stack.pushObject(null)
      stack.popCondition() shouldBe false
      stack.pushObject("not null")
      stack.popCondition() shouldBe true

      // Booleans
      stack.pushBoolean(false)
      stack.popCondition() shouldBe false
      stack.pushBoolean(true)
      stack.popCondition() shouldBe true

      // Integers
      stack.pushInt(0)
      stack.popCondition() shouldBe false
      stack.pushInt(41)
      stack.popCondition() shouldBe true

      // Longs
      stack.pushLong(0L)
      stack.popCondition() shouldBe false
      stack.pushLong(43L)
      stack.popCondition() shouldBe true

      // Shorts
      stack.pushShort(0.toShort)
      stack.popCondition() shouldBe false
      stack.pushShort(41.toShort)
      stack.popCondition() shouldBe true

      // Bytes
      stack.pushByte(0.toByte)
      stack.popCondition() shouldBe false
      stack.pushByte(43.toByte)
      stack.popCondition() shouldBe true

      // Chars
      stack.pushChar(0.toChar)
      stack.popCondition() shouldBe false
      stack.pushChar('A')
      stack.popCondition() shouldBe true

      // Doubles
      stack.pushDouble(0.0)
      stack.popCondition() shouldBe false
      stack.pushDouble(41.5)
      stack.popCondition() shouldBe true

      // Floats
      stack.pushFloat(0.0f)
      stack.popCondition() shouldBe false
      stack.pushFloat(43.5f)
      stack.popCondition() shouldBe true

      stack.isEmpty shouldBe true
    }
  }
}

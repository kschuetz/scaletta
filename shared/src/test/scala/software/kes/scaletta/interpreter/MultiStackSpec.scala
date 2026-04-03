package software.kes.scaletta.interpreter

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.common.BasicTypes
import software.kes.scaletta.runtime.{CoreTypes, FrameSignature}
import software.kes.scaletta.types.{Type, TypeId}

class MultiStackSpec extends AnyFunSpec with Matchers {
  describe("MultiStack") {
    it("should correctly expand and contract a frame") {
      val stack = MultiStack.create()
      val types: Seq[Type[TypeId]] = Seq(CoreTypes.IntT, CoreTypes.BooleanT, CoreTypes.IntT, CoreTypes.AnyRefT)
      val signature = FrameSignature.fromSeq(types)

      signature.intCount shouldBe 2
      signature.booleanCount shouldBe 1
      signature.objectCount shouldBe 1
      signature.floatCount shouldBe 0

      stack.expandFrame(signature)
      stack.size() shouldBe 4
      stack.isEmpty shouldBe false

      stack.ints.size() shouldBe 2
      stack.booleans.size() shouldBe 1
      stack.objects.size() shouldBe 1
      stack.floats.size() shouldBe 0

      stack.contractFrame(signature)
      stack.size() shouldBe 0
      stack.isEmpty shouldBe true

      stack.ints.size() shouldBe 0
      stack.booleans.size() shouldBe 0
      stack.objects.size() shouldBe 0
      stack.floats.size() shouldBe 0
    }

    it("should be initially empty") {
      val stack = MultiStack.create()
      stack.isEmpty shouldBe true
      stack.size() shouldBe 0
      stack.peek shouldBe None
      stack.peekBasicType shouldBe None
    }

    it("should push and pop objects correctly") {
      val stack = MultiStack.create()
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
      val stack = MultiStack.create()

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
      val stack = MultiStack.create()
      stack.push(41)
      stack.push("43")
      stack.push(true)

      stack.pop() shouldBe true
      stack.pop() shouldBe "43"
      stack.pop() shouldBe 41
    }

    it("should support unsafePop variants") {
      val stack = MultiStack.create()
      stack.pushInt(41)
      stack.pushObject("43")
      stack.pushBoolean(false)

      stack.unsafePopBoolean() shouldBe false
      stack.unsafePopObject() shouldBe "43"
      stack.unsafePopInt() shouldBe 41
      stack.isEmpty shouldBe true
    }

    it("should maintain LIFO order across different types") {
      val stack = MultiStack.create()
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
      val stack = MultiStack.create()
      stack.pushInt(41)
      stack.pushObject("43")
      stack.clear()

      stack.isEmpty shouldBe true
      stack.size() shouldBe 0
      stack.peek shouldBe None
    }

    it("should throw exception when popping from empty stack") {
      val stack = MultiStack.create()
      // The underlying stacks (like ByteStack) usually throw NoSuchElementException or similar
      // MultiStack.pop calls control.pop() first.
      assertThrows[NoSuchElementException] {
        stack.pop()
      }
    }

    it("should correctly handle many elements of different types") {
      val stack = MultiStack.create()
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

    it("should allow popping values from an expanded frame") {
      val stack = MultiStack.create()
      val signature = FrameSignature.of(CoreTypes.IntT, CoreTypes.BooleanT)
      stack.expandFrame(signature)

      stack.size() shouldBe 2

      // Manually initialize the values in the expanded slots
      // (Since expandFrame doesn't initialize them, we use unsafeWrite)
      stack.ints.unsafeWrite(0, 41)
      stack.booleans.unsafeWrite(0, true)

      stack.pop() shouldBe true
      stack.pop() shouldBe 41
      stack.isEmpty shouldBe true
    }
  }
}

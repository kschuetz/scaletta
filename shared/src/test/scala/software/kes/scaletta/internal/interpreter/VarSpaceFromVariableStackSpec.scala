package software.kes.scaletta.internal.interpreter

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.runtime.{CoreTypes, FrameSignature, VarSpaceSignature}

class VarSpaceFromVariableStackSpec extends AnyFunSpec with Matchers {
  describe("VarSpaceFromVariableStack") {
    it("should read and write values correctly") {
      val stack = VariableStack.create()
      val fs = FrameSignature.of(
        CoreTypes.IntT,
        CoreTypes.StringT,
        CoreTypes.BooleanT,
        CoreTypes.IntT
      )
      stack.expandFrame(fs)
      stack.ints.unsafeWrite(0, 41)
      stack.objects.unsafeWrite(0, "hello")
      stack.booleans.unsafeWrite(0, true)
      stack.ints.unsafeWrite(1, 43)

      val signature = VarSpaceSignature.of(fs)

      val varSpace = VarSpaceFromVariableStack.create(stack, signature)

      varSpace.read(0) shouldBe 41
      varSpace.read(1) shouldBe "hello"
      varSpace.read(2) shouldBe true
      varSpace.read(3) shouldBe 43

      varSpace.unsafeReadInt(0) shouldBe 41
      varSpace.unsafeReadObject(1) shouldBe "hello"
      varSpace.unsafeReadBoolean(2) shouldBe true
      varSpace.unsafeReadInt(3) shouldBe 43

      varSpace.unsafeWriteInt(0, 47)
      varSpace.unsafeWriteObject(1, "world")
      varSpace.unsafeWriteBoolean(2, false)
      varSpace.unsafeWriteInt(3, 53)

      varSpace.read(0) shouldBe 47
      varSpace.read(1) shouldBe "world"
      varSpace.read(2) shouldBe false
      varSpace.read(3) shouldBe 53
    }

    it("should handle all primitive types") {
      val stack = VariableStack.create()
      val fs = FrameSignature.of(
        CoreTypes.ByteT,
        CoreTypes.ShortT,
        CoreTypes.CharT,
        CoreTypes.LongT,
        CoreTypes.FloatT,
        CoreTypes.DoubleT
      )
      stack.expandFrame(fs)
      stack.bytes.unsafeWrite(0, 1)
      stack.shorts.unsafeWrite(0, 2)
      stack.chars.unsafeWrite(0, 'a')
      stack.longs.unsafeWrite(0, 3L)
      stack.floats.unsafeWrite(0, 4.5f)
      stack.doubles.unsafeWrite(0, 5.5)

      val signature = VarSpaceSignature.of(fs)

      val varSpace = VarSpaceFromVariableStack.create(stack, signature)

      varSpace.unsafeReadByte(0) shouldBe 1
      varSpace.unsafeReadShort(1) shouldBe 2
      varSpace.unsafeReadChar(2) shouldBe 'a'
      varSpace.unsafeReadLong(3) shouldBe 3L
      varSpace.unsafeReadFloat(4) shouldBe 4.5f
      varSpace.unsafeReadDouble(5) shouldBe 5.5

      varSpace.unsafeWriteByte(0, 10)
      varSpace.unsafeWriteShort(1, 20)
      varSpace.unsafeWriteChar(2, 'b')
      varSpace.unsafeWriteLong(3, 30L)
      varSpace.unsafeWriteFloat(4, 40.5f)
      varSpace.unsafeWriteDouble(5, 50.5)

      varSpace.unsafeReadByte(0) shouldBe 10
      varSpace.unsafeReadShort(1) shouldBe 20
      varSpace.unsafeReadChar(2) shouldBe 'b'
      varSpace.unsafeReadLong(3) shouldBe 30L
      varSpace.unsafeReadFloat(4) shouldBe 40.5f
      varSpace.unsafeReadDouble(5) shouldBe 50.5
    }
  }
}

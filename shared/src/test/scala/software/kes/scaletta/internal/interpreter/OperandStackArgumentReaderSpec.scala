package software.kes.scaletta.internal.interpreter

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.internal.runtime.{CoreTypes, ParamsSignature}

import scala.collection.immutable.ArraySeq

class OperandStackArgumentReaderSpec extends AnyFunSpec with Matchers {
  describe("OperandStackArgumentReader") {
    it("should read all primitive types correctly") {
      val stack = OperandStack.create()
      val signature = ParamsSignature.of(
        CoreTypes.BooleanT,
        CoreTypes.ByteT,
        CoreTypes.CharT,
        CoreTypes.ShortT,
        CoreTypes.IntT,
        CoreTypes.LongT,
        CoreTypes.FloatT,
        CoreTypes.DoubleT
      )

      stack.pushBoolean(true)
      stack.pushByte(43.toByte)
      stack.pushChar('Z')
      stack.pushShort(41.toShort)
      stack.pushInt(43)
      stack.pushLong(41L)
      stack.pushFloat(43.5f)
      stack.pushDouble(41.5)

      val reader = stack.argumentReader(signature)

      reader.argCount shouldBe 8

      reader.read(0) shouldBe true
      reader.readBoolean(0) shouldBe true

      reader.read(1) shouldBe 43.toByte
      reader.readByte(1) shouldBe 43.toByte

      reader.read(2) shouldBe 'Z'
      reader.readChar(2) shouldBe 'Z'

      reader.read(3) shouldBe 41.toShort
      reader.readShort(3) shouldBe 41.toShort

      reader.read(4) shouldBe 43
      reader.readInt(4) shouldBe 43

      reader.read(5) shouldBe 41L
      reader.readLong(5) shouldBe 41L

      reader.read(6) shouldBe 43.5f
      reader.readFloat(6) shouldBe 43.5f

      reader.read(7) shouldBe 41.5
      reader.readDouble(7) shouldBe 41.5
    }

    it("should read object types correctly") {
      val stack = OperandStack.create()
      val signature = ParamsSignature.of(CoreTypes.StringT, CoreTypes.AnyRefT)

      val s = "hello"
      val o = List(1, 2, 3)
      stack.pushObject(s)
      stack.pushObject(o)

      val reader = stack.argumentReader(signature)

      reader.read(0) shouldBe s
      reader.readObject(0) shouldBe s

      reader.read(1) shouldBe o
      reader.readObject(1) shouldBe o
    }

    it("should read primitive arrays correctly") {
      val stack = OperandStack.create()
      val signature = ParamsSignature.of(
        CoreTypes.AnyRefT,
        CoreTypes.AnyRefT,
        CoreTypes.AnyRefT,
        CoreTypes.AnyRefT,
        CoreTypes.AnyRefT,
        CoreTypes.AnyRefT,
        CoreTypes.AnyRefT,
        CoreTypes.AnyRefT
      )

      val booleans = ArraySeq(true, false, true)
      val bytes = ArraySeq(1.toByte, 2.toByte, 3.toByte)
      val chars = ArraySeq('A', 'B', 'C')
      val doubles = ArraySeq(41.5, 43.5)
      val floats = ArraySeq(41.5f, 43.5f)
      val ints = ArraySeq(41, 43)
      val longs = ArraySeq(41L, 43L)
      val shorts = ArraySeq(41.toShort, 43.toShort)

      stack.pushObject(booleans)
      stack.pushObject(bytes)
      stack.pushObject(chars)
      stack.pushObject(doubles)
      stack.pushObject(floats)
      stack.pushObject(ints)
      stack.pushObject(longs)
      stack.pushObject(shorts)

      val reader = stack.argumentReader(signature)

      reader.unsafeReadBooleanArray(0) shouldBe booleans
      reader.unsafeReadByteArray(1) shouldBe bytes
      reader.unsafeReadCharArray(2) shouldBe chars
      reader.unsafeReadDoubleArray(3) shouldBe doubles
      reader.unsafeReadFloatArray(4) shouldBe floats
      reader.unsafeReadIntArray(5) shouldBe ints
      reader.unsafeReadLongArray(6) shouldBe longs
      reader.unsafeReadShortArray(7) shouldBe shorts
    }

    it("should read mixed types with correct mapping (LIFO vs left-to-right)") {
      val stack = OperandStack.create()
      // Signature: (Int, String, Double, Boolean)
      val signature = ParamsSignature.of(CoreTypes.IntT, CoreTypes.StringT, CoreTypes.DoubleT, CoreTypes.BooleanT)

      // Arguments pushed left-to-right
      stack.pushInt(41)
      stack.pushObject("hello")
      stack.pushDouble(43.5)
      stack.pushBoolean(true)

      val reader = stack.argumentReader(signature)

      reader.readInt(0) shouldBe 41
      reader.readObject(1) shouldBe "hello"
      reader.readDouble(2) shouldBe 43.5
      reader.readBoolean(3) shouldBe true
    }

    it("should export to collections correctly") {
      val stack = OperandStack.create()
      val signature = ParamsSignature.of(CoreTypes.IntT, CoreTypes.BooleanT)
      stack.pushInt(41)
      stack.pushBoolean(true)

      val reader = stack.argumentReader(signature)
      reader.toVector shouldBe Vector(41, true)
      reader.toArray shouldBe Array[Any](41, true)
    }

    it("should handle empty signature") {
      val stack = OperandStack.create()
      val reader = stack.argumentReader(ParamsSignature.empty)

      reader.argCount shouldBe 0
      reader.toVector shouldBe Vector.empty
      reader.toArray shouldBe Array.empty[Any]
    }

    it("should handle multiple occurrences of the same type correctly") {
      val stack = OperandStack.create()
      val signature = ParamsSignature.of(CoreTypes.IntT, CoreTypes.IntT, CoreTypes.IntT)

      stack.pushInt(1)
      stack.pushInt(2)
      stack.pushInt(3)

      val reader = stack.argumentReader(signature)

      reader.readInt(0) shouldBe 1
      reader.readInt(1) shouldBe 2
      reader.readInt(2) shouldBe 3
    }

    describe("readX") {
      it("should read Boolean as Boolean") {
        val stack = OperandStack.create()
        val signature = ParamsSignature.of(CoreTypes.BooleanT)
        stack.pushBoolean(true)
        val reader = stack.argumentReader(signature)
        reader.readBoolean(0) shouldBe true
      }

      it("should read Int as Boolean (non-zero is true)") {
        val stack = OperandStack.create()
        val signature = ParamsSignature.of(CoreTypes.IntT, CoreTypes.IntT)
        stack.pushInt(41)
        stack.pushInt(0)
        val reader = stack.argumentReader(signature)
        reader.readBoolean(0) shouldBe true
        reader.readBoolean(1) shouldBe false
      }

      it("should read Int as Int") {
        val stack = OperandStack.create()
        val signature = ParamsSignature.of(CoreTypes.IntT)
        stack.pushInt(41)
        val reader = stack.argumentReader(signature)
        reader.readInt(0) shouldBe 41
      }

      it("should read Short as Int") {
        val stack = OperandStack.create()
        val signature = ParamsSignature.of(CoreTypes.ShortT)
        stack.pushShort(41.toShort)
        val reader = stack.argumentReader(signature)
        reader.readInt(0) shouldBe 41
      }

      it("should read Boolean as Int (true -> 1, false -> 0)") {
        val stack = OperandStack.create()
        val signature = ParamsSignature.of(CoreTypes.BooleanT, CoreTypes.BooleanT)
        stack.pushBoolean(true)
        stack.pushBoolean(false)
        val reader = stack.argumentReader(signature)
        reader.readInt(0) shouldBe 1
        reader.readInt(1) shouldBe 0
      }

      it("should read Object as Int via best-effort") {
        val stack = OperandStack.create()
        val signature = ParamsSignature.of(CoreTypes.StringT)
        stack.pushObject("41")
        val reader = stack.argumentReader(signature)
        // "41" is not a Number or Boolean, so it should be 0 according to ObjectToPrimitive
        reader.readInt(0) shouldBe 0
      }

      it("should read boxed Integer as Int") {
        val stack = OperandStack.create()
        val signature = ParamsSignature.of(CoreTypes.AnyRefT)
        stack.pushObject(java.lang.Integer.valueOf(43))
        val reader = stack.argumentReader(signature)
        reader.readInt(0) shouldBe 43
      }

      it("should read Int as boxed Integer via readAsObject") {
        val stack = OperandStack.create()
        val signature = ParamsSignature.of(CoreTypes.IntT)
        stack.pushInt(41)
        val reader = stack.argumentReader(signature)
        val result = reader.readObject(0)
        result shouldBe java.lang.Integer.valueOf(41)
        result.isInstanceOf[AnyRef] shouldBe true
      }
    }
  }
}

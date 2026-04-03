package software.kes.scaletta.runtime

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.common.BasicTypes

class FrameSignatureSpec extends AnyFunSpec with Matchers {
  describe("FrameSignature") {
    it("should correctly map types to stack offsets in forward order") {
      // (Int, String, Int, Double)
      // Slot 0: Int@0
      // Slot 1: String@0
      // Slot 2: Int@1
      // Slot 3: Double@0
      val sig = FrameSignature.of(CoreTypes.IntT, CoreTypes.StringT, CoreTypes.IntT, CoreTypes.DoubleT)

      sig.slotCount shouldBe 4

      sig.basicTypeOf(0) shouldBe BasicTypes.Int
      sig.stackOffsetOf(0) shouldBe 0

      sig.basicTypeOf(1) shouldBe BasicTypes.Object
      sig.stackOffsetOf(1) shouldBe 0

      sig.basicTypeOf(2) shouldBe BasicTypes.Int
      sig.stackOffsetOf(2) shouldBe 1

      sig.basicTypeOf(3) shouldBe BasicTypes.Double
      sig.stackOffsetOf(3) shouldBe 0
    }

    it("should support equality and hashCode") {
      val sig1 = FrameSignature.of(CoreTypes.IntT, CoreTypes.StringT)
      val sig2 = FrameSignature.of(CoreTypes.IntT, CoreTypes.StringT)
      val sig3 = FrameSignature.of(CoreTypes.StringT, CoreTypes.IntT)

      sig1 shouldBe sig2
      sig1.hashCode() shouldBe sig2.hashCode()

      sig1 shouldNot be(sig3)
      sig1 shouldNot be(ParamsSignature.of(CoreTypes.IntT, CoreTypes.StringT))
    }

    it("should handle empty signature") {
      val sig = FrameSignature.empty
      sig.slotCount shouldBe 0
      sig.slots shouldBe empty
      sig.toString shouldBe "FrameSignature()"
    }

    it("should handle all basic types") {
      val sig = FrameSignature.of(
        CoreTypes.AnyRefT,
        CoreTypes.BooleanT,
        CoreTypes.IntT,
        CoreTypes.LongT,
        CoreTypes.ShortT,
        CoreTypes.ByteT,
        CoreTypes.CharT,
        CoreTypes.DoubleT,
        CoreTypes.FloatT
      )

      sig.slotCount shouldBe 9
      for (i <- 0 until 9) {
        sig.stackOffsetOf(i) shouldBe 0
      }

      sig.basicTypeOf(0) shouldBe BasicTypes.Object
      sig.basicTypeOf(1) shouldBe BasicTypes.Boolean
      sig.basicTypeOf(2) shouldBe BasicTypes.Int
      sig.basicTypeOf(3) shouldBe BasicTypes.Long
      sig.basicTypeOf(4) shouldBe BasicTypes.Short
      sig.basicTypeOf(5) shouldBe BasicTypes.Byte
      sig.basicTypeOf(6) shouldBe BasicTypes.Char
      sig.basicTypeOf(7) shouldBe BasicTypes.Double
      sig.basicTypeOf(8) shouldBe BasicTypes.Float
    }

    it("should correctly round-trip via VarAddress") {
      val sig = FrameSignature.of(CoreTypes.IntT)
      val encoded = sig.slot(0)
      VarAddress.decodeBasicType(encoded) shouldBe BasicTypes.Int
      VarAddress.decodeStackOffset(encoded) shouldBe 0
    }
  }
}

package software.kes.scaletta.internal.runtime

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.common.BasicTypes

class ParamsSignatureSpec extends AnyFunSpec with Matchers {
  describe("ParamsSignature") {
    it("should correctly map types to stack offsets in reverse order (LIFO)") {
      // (Int, String, Int, Double)
      // Arguments are pushed left-to-right: Int(0), String(1), Int(2), Double(3)
      // On stack (top first): Double(3), Int(2), String(1), Int(0)
      // Double(3) is at offset 0 of Double stack
      // Int(2) is at offset 0 of Int stack
      // Int(0) is at offset 1 of Int stack
      // String(1) is at offset 0 of Object stack
      val sig = ParamsSignature.of(CoreTypes.IntT, CoreTypes.StringT, CoreTypes.IntT, CoreTypes.DoubleT)

      sig.paramCount shouldBe 4

      sig.basicTypeOf(0) shouldBe BasicTypes.Int
      sig.stackOffsetOf(0) shouldBe 1

      sig.basicTypeOf(1) shouldBe BasicTypes.Object
      sig.stackOffsetOf(1) shouldBe 0

      sig.basicTypeOf(2) shouldBe BasicTypes.Int
      sig.stackOffsetOf(2) shouldBe 0

      sig.basicTypeOf(3) shouldBe BasicTypes.Double
      sig.stackOffsetOf(3) shouldBe 0
    }

    it("should support equality and hashCode") {
      val sig1 = ParamsSignature.of(CoreTypes.IntT, CoreTypes.StringT)
      val sig2 = ParamsSignature.of(CoreTypes.IntT, CoreTypes.StringT)
      val sig3 = ParamsSignature.of(CoreTypes.StringT, CoreTypes.IntT)

      sig1 shouldBe sig2
      sig1.hashCode() shouldBe sig2.hashCode()

      sig1 shouldNot be(sig3)
      sig1 shouldNot be(FrameSignature.of(CoreTypes.IntT, CoreTypes.StringT))
    }

    it("should handle empty signature") {
      val sig = ParamsSignature.empty
      sig.paramCount shouldBe 0
      sig.params shouldBe empty
      sig.toString shouldBe "ParamsSignature()"
    }

    it("should handle all basic types") {
      val sig = ParamsSignature.of(
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

      sig.paramCount shouldBe 9
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
      val sig = ParamsSignature.of(CoreTypes.IntT)
      val encoded = sig.param(0)
      VarAddress.decodeBasicType(encoded) shouldBe BasicTypes.Int
      VarAddress.decodeStackOffset(encoded) shouldBe 0
    }
  }
}

package software.kes.scaletta.internal.runtime

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.common.BasicTypes

class VarSpaceSignatureSpec extends AnyFunSpec with Matchers {
  describe("VarSpaceSignature") {
    it("should handle empty frames") {
      val sig = VarSpaceSignature.fromSeq(Nil)
      sig shouldBe VarSpaceSignature.empty
      sig.slotCount shouldBe 0
      sig.toString shouldBe "VarSpaceSignature()"
    }

    it("should handle a single frame") {
      val f1 = FrameSignature.of(CoreTypes.IntT, CoreTypes.StringT, CoreTypes.IntT)
      val sig = VarSpaceSignature.of(f1)

      sig.slotCount shouldBe 3
      sig.basicTypeOf(0) shouldBe BasicTypes.Int
      sig.stackOffsetOf(0) shouldBe 0
      sig.basicTypeOf(1) shouldBe BasicTypes.Object
      sig.stackOffsetOf(1) shouldBe 0
      sig.basicTypeOf(2) shouldBe BasicTypes.Int
      sig.stackOffsetOf(2) shouldBe 1
    }

    it("should accumulate offsets across multiple frames") {
      val f1 = FrameSignature.of(CoreTypes.IntT, CoreTypes.StringT)
      val f2 = FrameSignature.of(CoreTypes.IntT, CoreTypes.DoubleT)
      val f3 = FrameSignature.of(CoreTypes.StringT, CoreTypes.IntT)

      val sig = VarSpaceSignature.of(f1, f2, f3)

      sig.slotCount shouldBe 6

      // Frame 1 slots
      sig.basicTypeOf(0) shouldBe BasicTypes.Int
      sig.stackOffsetOf(0) shouldBe 0
      sig.basicTypeOf(1) shouldBe BasicTypes.Object
      sig.stackOffsetOf(1) shouldBe 0

      // Frame 2 slots
      sig.basicTypeOf(2) shouldBe BasicTypes.Int
      sig.stackOffsetOf(2) shouldBe 1
      sig.basicTypeOf(3) shouldBe BasicTypes.Double
      sig.stackOffsetOf(3) shouldBe 0

      // Frame 3 slots
      sig.basicTypeOf(4) shouldBe BasicTypes.Object
      sig.stackOffsetOf(4) shouldBe 1
      sig.basicTypeOf(5) shouldBe BasicTypes.Int
      sig.stackOffsetOf(5) shouldBe 2
    }

    it("should handle all basic types correctly across frames") {
      val allTypes = Seq(
        CoreTypes.BooleanT,
        CoreTypes.IntT,
        CoreTypes.LongT,
        CoreTypes.ShortT,
        CoreTypes.ByteT,
        CoreTypes.CharT,
        CoreTypes.DoubleT,
        CoreTypes.FloatT,
        CoreTypes.AnyRefT
      )
      val f1 = FrameSignature.fromSeq(allTypes)
      val f2 = FrameSignature.fromSeq(allTypes)

      val sig = VarSpaceSignature.of(f1, f2)

      sig.slotCount shouldBe 18
      for (i <- 0 until 9) {
        sig.stackOffsetOf(i) shouldBe 0
        sig.stackOffsetOf(i + 9) shouldBe 1
        sig.basicTypeOf(i) shouldBe sig.basicTypeOf(i + 9)
      }
    }

    it("should support equality and hashCode") {
      val f1 = FrameSignature.of(CoreTypes.IntT)
      val f2 = FrameSignature.of(CoreTypes.StringT)

      val sig1 = VarSpaceSignature.of(f1, f2)
      val sig2 = VarSpaceSignature.of(f1, f2)
      val sig3 = VarSpaceSignature.of(f2, f1)

      sig1 shouldBe sig2
      sig1.hashCode() shouldBe sig2.hashCode()

      sig1 shouldNot be(sig3)
      sig1 shouldNot be(f1)
    }

    it("should not be equal to a FrameSignature with same slots") {
      val f1 = FrameSignature.of(CoreTypes.IntT)
      val sig = VarSpaceSignature.of(f1)

      sig.slot(0) shouldBe f1.slot(0)

      sig shouldNot be(f1)
      f1 shouldNot be(sig)
    }

    it("should have a helpful toString") {
      val f1 = FrameSignature.of(CoreTypes.IntT, CoreTypes.StringT)
      val sig = VarSpaceSignature.of(f1)
      sig.toString shouldBe "VarSpaceSignature(Int, Object)"
    }

    it("should correctly provide slot by index") {
      val f1 = FrameSignature.of(CoreTypes.IntT)
      val sig = VarSpaceSignature.of(f1)
      val expected = VarAddress.encode(BasicTypes.Int, 0)
      sig.slot(0) shouldBe expected
    }

    it("should store the first frame signature") {
      val f1 = FrameSignature.of(CoreTypes.IntT, CoreTypes.StringT)
      val f2 = FrameSignature.of(CoreTypes.DoubleT)
      val sig = VarSpaceSignature.of(f1, f2)

      sig.frameSignature shouldBe f1
    }

    it("should use FrameSignature.empty when no frames are provided") {
      val sig = VarSpaceSignature.fromSeq(Nil)
      sig.frameSignature shouldBe FrameSignature.empty
    }

    describe("pushFrame") {
      it("should prepend the new frame and shift offsets") {
        val f1 = FrameSignature.of(CoreTypes.IntT, CoreTypes.StringT)
        val sig1 = VarSpaceSignature.of(f1)

        val f2 = FrameSignature.of(CoreTypes.IntT, CoreTypes.DoubleT)
        val sig2 = sig1.pushFrame(f2)

        sig2.slotCount shouldBe 4

        // New frame slots (f2) at index 0 and 1
        sig2.basicTypeOf(0) shouldBe BasicTypes.Int
        sig2.stackOffsetOf(0) shouldBe 0
        sig2.basicTypeOf(1) shouldBe BasicTypes.Double
        sig2.stackOffsetOf(1) shouldBe 0

        // Old frame slots (f1) shifted
        sig2.basicTypeOf(2) shouldBe BasicTypes.Int
        sig2.stackOffsetOf(2) shouldBe 1 // shifted by 1 (f2 has 1 Int)
        sig2.basicTypeOf(3) shouldBe BasicTypes.Object
        sig2.stackOffsetOf(3) shouldBe 0 // not shifted (f2 has 0 Objects)
      }

      it("should update the frameSignature to the pushed frame") {
        val f1 = FrameSignature.of(CoreTypes.IntT)
        val sig1 = VarSpaceSignature.of(f1)
        val f2 = FrameSignature.of(CoreTypes.DoubleT)
        val sig2 = sig1.pushFrame(f2)

        sig2.frameSignature shouldBe f2
      }

      it("should work correctly when pushing onto an empty signature") {
        val f1 = FrameSignature.of(CoreTypes.IntT, CoreTypes.IntT)
        val sig = VarSpaceSignature.empty.pushFrame(f1)

        sig.slotCount shouldBe 2
        sig.basicTypeOf(0) shouldBe BasicTypes.Int
        sig.stackOffsetOf(0) shouldBe 0
        sig.basicTypeOf(1) shouldBe BasicTypes.Int
        sig.stackOffsetOf(1) shouldBe 1
        sig.frameSignature shouldBe f1
      }
    }
  }
}

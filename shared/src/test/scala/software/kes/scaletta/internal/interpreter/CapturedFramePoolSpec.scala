package software.kes.scaletta.internal.interpreter

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.common.BasicTypes
import software.kes.scaletta.internal.runtime.VarAddress

class CapturedFramePoolSpec extends AnyFunSpec with Matchers {
  describe("CapturedFramePool") {
    val sig1 = CaptureSignature.create(objectCount = 1, booleanCount = 0, intCount = 1, longCount = 0, shortCount = 0, byteCount = 0, charCount = 0, doubleCount = 0, floatCount = 0)
    val sig2 = CaptureSignature.create(objectCount = 0, booleanCount = 0, intCount = 2, longCount = 0, shortCount = 0, byteCount = 0, charCount = 0, doubleCount = 0, floatCount = 0)

    it("should acquire new frames when pool is empty") {
      val pool = new CapturedFramePool(maxRetained = 2)
      val frame1 = pool.acquire(sig1)
      frame1.signature shouldBe sig1
      frame1.objects.length shouldBe 1
      frame1.ints.length shouldBe 1
    }

    it("should reuse frames with the same signature after endRun") {
      val pool = new CapturedFramePool(maxRetained = 2)
      val frame1 = pool.acquire(sig1)
      pool.endRun()

      val frame2 = pool.acquire(sig1)
      frame2 should be theSameInstanceAs frame1
    }

    it("should not reuse frames with different signatures") {
      val pool = new CapturedFramePool(maxRetained = 2)
      val frame1 = pool.acquire(sig1)
      pool.endRun()

      val frame2 = pool.acquire(sig2)
      frame2 shouldNot be theSameInstanceAs frame1
      frame2.signature shouldBe sig2
    }

    it("should clear object references on endRun") {
      val pool = new CapturedFramePool(maxRetained = 2)
      val frame1 = pool.acquire(sig1)
      val obj = new Object()
      frame1.objects(0) = obj

      pool.endRun()

      frame1.objects(0) shouldBe null
    }

    it("should respect maxRetained cap") {
      val pool = new CapturedFramePool(maxRetained = 1)
      val frame1 = pool.acquire(sig1)
      val frame2 = pool.acquire(sig1)

      pool.endRun()
      // Only one should be retained in 'free'

      val frame3 = pool.acquire(sig1)
      val frame4 = pool.acquire(sig1)

      (frame3 eq frame1) || (frame3 eq frame2) shouldBe true
      // The one that was not retained should be a new instance
      if (frame3 eq frame1) frame4 shouldNot be theSameInstanceAs frame2
      else frame4 shouldNot be theSameInstanceAs frame1
    }

    it("should clear all frames on clear") {
      val pool = new CapturedFramePool(maxRetained = 2)
      val frame1 = pool.acquire(sig1)
      pool.endRun()
      pool.clear()

      val frame2 = pool.acquire(sig1)
      frame2 shouldNot be theSameInstanceAs frame1
    }

    it("should provide typed backing arrays") {
      val sig = CaptureSignature.create(1, 1, 1, 1, 1, 1, 1, 1, 1)
      val frame = CapturedFrame.create(sig)
      frame.objects.length shouldBe 1
      frame.booleans.length shouldBe 1
      frame.ints.length shouldBe 1
      frame.longs.length shouldBe 1
      frame.shorts.length shouldBe 1
      frame.bytes.length shouldBe 1
      frame.chars.length shouldBe 1
      frame.doubles.length shouldBe 1
      frame.floats.length shouldBe 1
    }

    it("should capture variables from VarSpaceFromVariableStack") {
      val stack = VariableStack.create()
      // We need to create a VarSpaceSignature that matches our data
      val frameSig = software.kes.scaletta.internal.runtime.FrameSignature.fromSeq(
        List(
          software.kes.scaletta.internal.runtime.CoreTypes.IntT,
          software.kes.scaletta.internal.runtime.CoreTypes.StringT
        )
      )
      val signature = software.kes.scaletta.internal.runtime.VarSpaceSignature.of(frameSig)
      val varSpace = VarSpaceFromVariableStack.create(stack, signature)
      stack.expandFrame(frameSig)

      varSpace.unsafeWriteInt(0, 41)
      varSpace.unsafeWriteObject(1, "43")

      val capSig = CaptureSignature.create(objectCount = 1, booleanCount = 0, intCount = 1, longCount = 0, shortCount = 0, byteCount = 0, charCount = 0, doubleCount = 0, floatCount = 0)
      val target = CapturedFrame.create(capSig)

      val plan = new CapturePlan(
        signature = capSig,
        sourceIndices = Array(0, 1),
        targetEncoded = Array(
          VarAddress.encode(BasicTypes.Int, 0),
          VarAddress.encode(BasicTypes.Object, 0)
        )
      )

      plan.capture(varSpace, target)

      target.ints(0) shouldBe 41
      target.objects(0) shouldBe "43"
    }
  }
}

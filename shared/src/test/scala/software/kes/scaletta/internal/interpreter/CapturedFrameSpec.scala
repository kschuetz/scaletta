package software.kes.scaletta.internal.interpreter

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class CapturedFrameSpec extends AnyFunSpec with Matchers {
  describe("CapturedFrame") {
    it("should provide an empty frame") {
      val frame = CapturedFrame.empty
      frame.isEmpty shouldBe true
      frame.signature shouldBe CaptureSignature.empty
      frame.objects shouldBe null
      frame.booleans shouldBe null
      frame.ints shouldBe null
      frame.longs shouldBe null
      frame.shorts shouldBe null
      frame.bytes shouldBe null
      frame.chars shouldBe null
      frame.doubles shouldBe null
      frame.floats shouldBe null
    }

    it("should create a frame with correct array sizes based on signature") {
      val signature = CaptureSignature.create(
        objectCount = 1,
        booleanCount = 2,
        intCount = 3,
        longCount = 4,
        shortCount = 5,
        byteCount = 6,
        charCount = 7,
        doubleCount = 8,
        floatCount = 9
      )
      val frame = CapturedFrame.create(signature)

      frame.isEmpty shouldBe false
      frame.signature shouldBe signature
      frame.objects.length shouldBe 1
      frame.booleans.length shouldBe 2
      frame.ints.length shouldBe 3
      frame.longs.length shouldBe 4
      frame.shorts.length shouldBe 5
      frame.bytes.length shouldBe 6
      frame.chars.length shouldBe 7
      frame.doubles.length shouldBe 8
      frame.floats.length shouldBe 9
    }

    it("should return empty frame when signature is empty") {
      CapturedFrame.create(CaptureSignature.empty) shouldBe CapturedFrame.empty
    }

    it("should clear objects correctly") {
      val signature = CaptureSignature.create(objectCount = 2, 0, 0, 0, 0, 0, 0, 0, 0)
      val frame = CapturedFrame.create(signature)
      frame.objects(0) = "first"
      frame.objects(1) = "second"

      frame.clearObjects()

      frame.objects(0) shouldBe null
      frame.objects(1) shouldBe null
    }

    it("should not fail clearObjects on empty frame") {
      val frame = CapturedFrame.empty
      noException should be thrownBy frame.clearObjects()
    }
  }
}

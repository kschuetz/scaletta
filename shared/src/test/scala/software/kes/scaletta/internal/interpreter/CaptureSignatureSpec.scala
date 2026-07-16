package software.kes.scaletta.internal.interpreter

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class CaptureSignatureSpec extends AnyFunSpec with Matchers {
  describe("CaptureSignature") {
    it("should provide an empty signature") {
      val sig = CaptureSignature.empty
      sig.isEmpty shouldBe true
      sig.objectCount shouldBe 0
      sig.booleanCount shouldBe 0
      sig.intCount shouldBe 0
      sig.longCount shouldBe 0
      sig.shortCount shouldBe 0
      sig.byteCount shouldBe 0
      sig.charCount shouldBe 0
      sig.doubleCount shouldBe 0
      sig.floatCount shouldBe 0
    }

    it("should create a concrete signature when counts are positive") {
      val sig = CaptureSignature.create(
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
      sig.isEmpty shouldBe false
      sig.objectCount shouldBe 1
      sig.booleanCount shouldBe 2
      sig.intCount shouldBe 3
      sig.longCount shouldBe 4
      sig.shortCount shouldBe 5
      sig.byteCount shouldBe 6
      sig.charCount shouldBe 7
      sig.doubleCount shouldBe 8
      sig.floatCount shouldBe 9
    }

    it("should return Empty when all counts are zero or negative") {
      CaptureSignature.create(0, 0, 0, 0, 0, 0, 0, 0, 0) shouldBe CaptureSignature.empty
      CaptureSignature.create(-1, -1, -1, -1, -1, -1, -1, -1, -1) shouldBe CaptureSignature.empty
    }

    it("should support equality and hashCode") {
      val sig1 = CaptureSignature.create(1, 0, 2, 0, 3, 0, 4, 0, 5)
      val sig2 = CaptureSignature.create(1, 0, 2, 0, 3, 0, 4, 0, 5)
      val sig3 = CaptureSignature.create(0, 1, 0, 2, 0, 3, 0, 4, 0)

      sig1 shouldEqual sig2
      sig1.hashCode() shouldEqual sig2.hashCode()

      sig1 shouldNot equal(sig3)
      sig1 shouldNot equal(CaptureSignature.empty)
    }

    it("should have a sensible toString") {
      CaptureSignature.empty.toString shouldBe "CaptureSignature(object=0, boolean=0, int=0, long=0, short=0, byte=0, char=0, double=0, float=0)"
      val sig = CaptureSignature.create(1, 2, 3, 4, 5, 6, 7, 8, 9)
      sig.toString shouldBe "CaptureSignature(object=1, boolean=2, int=3, long=4, short=5, byte=6, char=7, double=8, float=9)"
    }
  }
}

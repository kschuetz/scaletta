package software.kes.scaletta.internal.interpreter

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class CapturedFrameOptimizationSpec extends AnyFunSpec with Matchers {
  describe("CapturedFrame Optimization") {
    val sig1 = CaptureSignature.create(objectCount = 1, 0, intCount = 1, 0, 0, 0, 0, 0, 0)
    val sig2 = CaptureSignature.create(objectCount = 2, 0, intCount = 2, 0, 0, 0, 0, 0, 0)
    val sigBig = CaptureSignature.create(objectCount = 100, 0, 0, 0, 0, 0, 0, 0, 0) // Over MaxArrayCap=64

    it("should morph frames through setSignature") {
      val frame = CapturedFrame.create(sig1)
      frame.objects.length shouldBe 1
      frame.ints.length shouldBe 1

      frame.setSignature(sig2)
      frame.signature shouldBe sig2
      frame.objects.length should be >= 2
      frame.ints.length should be >= 2
    }

    it("should reuse frames from signature buckets") {
      val pool = new CapturedFramePool(maxRetained = 2)
      val frame1 = pool.acquire(sig1)
      pool.endRun()

      val frame2 = pool.acquire(sig1)
      frame2 should be theSameInstanceAs frame1
      pool.hits shouldBe 1
    }

    it("should reuse frames from generic spare by morphing") {
      val pool = new CapturedFramePool(maxRetained = 2)

      // sigBig is over cap (64), so it should go to generic spare
      val frameBig = pool.acquire(sigBig)
      pool.endRun()

      pool.totalFree shouldBe 1

      val frame2 = pool.acquire(sig1)
      frame2 should be theSameInstanceAs frameBig
      frame2.signature shouldBe sig1
      pool.genericHits shouldBe 1
    }

    it("should evict from largest bucket when at capacity") {
      val pool = new CapturedFramePool(maxRetained = 3)

      // Fill pool with 3 different signatures
      val s1 = CaptureSignature.create(1, 0, 0, 0, 0, 0, 0, 0, 0)
      val s2 = CaptureSignature.create(2, 0, 0, 0, 0, 0, 0, 0, 0)
      val s3 = CaptureSignature.create(3, 0, 0, 0, 0, 0, 0, 0, 0)

      // Acquire and release them
      // We need to do this carefully because endRun clears borrowed.
      // We'll acquire all and then endRun.
      val f1a = pool.acquire(s1)
      val f1b = pool.acquire(s1)
      val f2 = pool.acquire(s2)
      val f3 = pool.acquire(s3)

      pool.endRun()

      // Total 4 frames, but maxRetained is 3.
      // Buckets: s1(2), s2(1), s3(1)
      // Evict from s1.
      // Remaining: s1(1), s2(1), s3(1)

      pool.totalFree shouldBe 3
      pool.evictions shouldBe 1

      // Check that s1 now has only 1 frame in pool
      val r1 = pool.acquire(s1)
      val r2 = pool.acquire(s1)

      (r1 eq f1a) || (r1 eq f1b) shouldBe true
      (r2 eq f1a) || (r2 eq f1b) shouldBe false // The other one was evicted
    }

    it("should clear object references on endRun") {
      val pool = new CapturedFramePool(maxRetained = 1)
      val frame = pool.acquire(sig1)
      frame.objects(0) = "leak"
      pool.endRun()

      frame.objects(0) shouldBe null
    }
  }
}

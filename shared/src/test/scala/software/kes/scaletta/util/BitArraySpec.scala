package software.kes.scaletta.util

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class BitArraySpec extends AnyFunSpec with Matchers {
  describe("BitArray") {
    it("should be initially empty when created with capacity 0") {
      val bits = BitArray.create(0)
      bits.get(0) shouldBe false
      bits.get(100) shouldBe false
    }

    it("should set and get bits correctly") {
      val bits = BitArray.create(10)
      bits.set(5)
      bits.get(5) shouldBe true
      bits.get(4) shouldBe false
      bits.get(6) shouldBe false
    }

    it("should clear bits correctly") {
      val bits = BitArray.create(10)
      bits.set(5)
      bits.get(5) shouldBe true
      bits.clear(5)
      bits.get(5) shouldBe false
    }

    it("should update bits correctly") {
      val bits = BitArray.create(10)
      bits.update(5, true)
      bits.get(5) shouldBe true
      bits.update(5, false)
      bits.get(5) shouldBe false
    }

    it("should grow automatically when setting a bit beyond capacity") {
      val bits = BitArray.create(10)
      bits.set(100) // This should trigger growth
      bits.get(100) shouldBe true
      bits.get(50) shouldBe false
    }

    it("should handle bits across 64-bit boundaries") {
      val bits = BitArray.create(128)
      bits.set(63)
      bits.set(64)
      bits.get(63) shouldBe true
      bits.get(64) shouldBe true
      bits.get(62) shouldBe false
      bits.get(65) shouldBe false
    }

    it("should handle large indices") {
      val bits = BitArray.create(0)
      bits.set(1000)
      bits.get(1000) shouldBe true
      bits.get(999) shouldBe false
      bits.get(1001) shouldBe false
    }

    it("should support manual capacity management") {
      val bits = BitArray.create(10)
      bits.bitCapacity() should be >= 10
      bits.ensureCapacity(200)
      bits.bitCapacity() should be >= 200
      bits.set(150)
      bits.get(150) shouldBe true
    }

    it("should not shrink when ensureCapacity is called with smaller value") {
      val bits = BitArray.create(100)
      val initialCapacity = bits.bitCapacity()
      bits.ensureCapacity(10)
      bits.bitCapacity() shouldBe initialCapacity
    }
  }
}

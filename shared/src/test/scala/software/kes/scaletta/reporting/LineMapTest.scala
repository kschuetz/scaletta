package software.kes.scaletta.reporting

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class LineMapTest extends AnyFunSpec with Matchers {
  describe("LineMap") {
    it("should create a LineMap with default base position (1, 1)") {
      val lm = LineMap.create()
      lm.indexToPosition(CharIndex(0)) shouldBe Position.first
      lm.indexToPosition(CharIndex(10)) shouldBe Position.of(1, 11)
    }

    it("should create a LineMap with a custom base position") {
      val base = Position.of(11, 6)
      val lm = LineMap.create(base)
      lm.indexToPosition(CharIndex(0)) shouldBe Position.of(11, 6)
      lm.indexToPosition(CharIndex(5)) shouldBe Position.of(11, 11)
    }

    it("should correctly handle multiple lines with custom base position") {
      val base = Position.of(11, 6)
      val lm = LineMap.create(base)
        .addLineBegin(CharIndex(10)) // Line 12 starts at index 10

      // Line 11: [0, 9], starts at column 6
      lm.indexToPosition(CharIndex(0)) shouldBe Position.of(11, 6)
      lm.indexToPosition(CharIndex(9)) shouldBe Position.of(11, 15)

      // Line 12: [10, ...], starts at column 1
      lm.indexToPosition(CharIndex(10)) shouldBe Position.of(12, 1)
      lm.indexToPosition(CharIndex(15)) shouldBe Position.of(12, 6)
    }

    it("should correctly handle multiple lines") {
      val lm = LineMap.create()
        .addLineBegin(CharIndex(10)) // Line 2 starts at index 10
        .addLineBegin(CharIndex(20)) // Line 3 starts at index 20

      // Line 1: [0, 9]
      lm.indexToPosition(CharIndex(0)) shouldBe Position.first
      lm.indexToPosition(CharIndex(5)) shouldBe Position.of(1, 6)
      lm.indexToPosition(CharIndex(9)) shouldBe Position.of(1, 10)

      // Line 2: [10, 19]
      lm.indexToPosition(CharIndex(10)) shouldBe Position.of(2, 1)
      lm.indexToPosition(CharIndex(15)) shouldBe Position.of(2, 6)
      lm.indexToPosition(CharIndex(19)) shouldBe Position.of(2, 10)

      // Line 3: [20, ...]
      lm.indexToPosition(CharIndex(20)) shouldBe Position.of(3, 1)
      lm.indexToPosition(CharIndex(25)) shouldBe Position.of(3, 6)
    }

    it("should handle large gaps between lines") {
      val lm = LineMap.create().addLineBegin(CharIndex(1000))
      lm.indexToPosition(CharIndex(0)) shouldBe Position.first
      lm.indexToPosition(CharIndex(999)) shouldBe Position.of(1, 1000)
      lm.indexToPosition(CharIndex(1000)) shouldBe Position.of(2, 1)
      lm.indexToPosition(CharIndex(1500)) shouldBe Position.of(2, 501)
    }

    it("should handle adding line begins at same index (should be idempotent)") {
      val lm = LineMap.create()
        .addLineBegin(CharIndex(10)) // Line 2 starts at 10
        .addLineBegin(CharIndex(10)) // Should be ignored

      lm.indexToPosition(CharIndex(10)) shouldBe Position.of(2, 1)
    }

    it("should ignore out-of-order line begins") {
      val lm = LineMap.create()
        .addLineBegin(CharIndex(20))
        .addLineBegin(CharIndex(10))

      lm.indexToPosition(CharIndex(20)) shouldBe Position.of(2, 1)
      lm.indexToPosition(CharIndex(10)) shouldBe Position.of(1, 11)
    }

    it("should handle indices before the first explicitly added line begin") {
      val lm = LineMap.create().addLineBegin(CharIndex(5))
      lm.indexToPosition(CharIndex(0)) shouldBe Position.first
      lm.indexToPosition(CharIndex(4)) shouldBe Position.of(1, 5)
      lm.indexToPosition(CharIndex(5)) shouldBe Position.of(2, 1)
    }
  }

  describe("LineMapBuilder") {
    it("should build a LineMap correctly") {
      val builder = LineMap.create().builder
      builder.addLineBegin(CharIndex(10))
      builder.addLineBegin(CharIndex(20))

      val lm = builder.result
      lm.indexToPosition(CharIndex(0)) shouldBe Position.first
      lm.indexToPosition(CharIndex(10)) shouldBe Position.of(2, 1)
      lm.indexToPosition(CharIndex(20)) shouldBe Position.of(3, 1)
    }

    it("should enforce monotonicity") {
      val builder = LineMap.create().builder
      builder.addLineBegin(CharIndex(10))
      val lm1 = builder.result
      lm1.indexToPosition(CharIndex(10)) shouldBe Position.of(2, 1)

      // Should ignore out-of-order or duplicate indices
      builder.addLineBegin(CharIndex(10))
      builder.addLineBegin(CharIndex(5))
      builder.result shouldBe lm1

      builder.addLineBegin(CharIndex(20))
      builder.result.indexToPosition(CharIndex(20)) shouldBe Position.of(3, 1)
    }

    it("should initialize monotonicity tracking from an existing LineMap") {
      val initial = LineMap.create().addLineBegin(CharIndex(10))
      val builder = initial.builder

      // Should ignore index 10 since lastIndex should be 10
      builder.addLineBegin(CharIndex(10))
      builder.result shouldBe initial

      // Should ignore index 5
      builder.addLineBegin(CharIndex(5))
      builder.result shouldBe initial

      // Should accept index 11
      builder.addLineBegin(CharIndex(11))
      builder.result.indexToPosition(CharIndex(11)) shouldBe Position.of(3, 1)
    }
  }
}

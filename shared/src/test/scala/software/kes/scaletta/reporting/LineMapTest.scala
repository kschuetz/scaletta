package software.kes.scaletta.reporting

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class LineMapTest extends AnyFunSpec with Matchers {
  describe("LineMap") {
    it("should create a LineMap with default base position (0, 0)") {
      val lm = LineMap.create()
      lm.indexToPosition(CharIndex(0)) shouldBe Position(LineIndex(0), ColumnIndex(0))
      lm.indexToPosition(CharIndex(10)) shouldBe Position(LineIndex(0), ColumnIndex(10))
    }

    it("should create a LineMap with a custom base position") {
      val base = Position(LineIndex(10), ColumnIndex(5))
      val lm = LineMap.create(base)
      // Note: indexToPosition implementation currently ignores base column when calculating from base line.
      // Let's check how it handles index 0.
      lm.indexToPosition(CharIndex(0)) shouldBe Position(LineIndex(10), ColumnIndex(0))
      lm.indexToPosition(CharIndex(5)) shouldBe Position(LineIndex(10), ColumnIndex(5))
    }

    it("should correctly handle multiple lines") {
      val lm = LineMap.create()
        .addLineBegin(CharIndex(10)) // Line 1 starts at index 10
        .addLineBegin(CharIndex(20)) // Line 2 starts at index 20

      // Line 0: [0, 9]
      lm.indexToPosition(CharIndex(0)) shouldBe Position(LineIndex(0), ColumnIndex(0))
      lm.indexToPosition(CharIndex(5)) shouldBe Position(LineIndex(0), ColumnIndex(5))
      lm.indexToPosition(CharIndex(9)) shouldBe Position(LineIndex(0), ColumnIndex(9))

      // Line 1: [10, 19]
      lm.indexToPosition(CharIndex(10)) shouldBe Position(LineIndex(1), ColumnIndex(0))
      lm.indexToPosition(CharIndex(15)) shouldBe Position(LineIndex(1), ColumnIndex(5))
      lm.indexToPosition(CharIndex(19)) shouldBe Position(LineIndex(1), ColumnIndex(9))

      // Line 2: [20, ...]
      lm.indexToPosition(CharIndex(20)) shouldBe Position(LineIndex(2), ColumnIndex(0))
      lm.indexToPosition(CharIndex(25)) shouldBe Position(LineIndex(2), ColumnIndex(5))
    }

    it("should handle large gaps between lines") {
      val lm = LineMap.create().addLineBegin(CharIndex(1000))
      lm.indexToPosition(CharIndex(0)) shouldBe Position(LineIndex(0), ColumnIndex(0))
      lm.indexToPosition(CharIndex(999)) shouldBe Position(LineIndex(0), ColumnIndex(999))
      lm.indexToPosition(CharIndex(1000)) shouldBe Position(LineIndex(1), ColumnIndex(0))
      lm.indexToPosition(CharIndex(1500)) shouldBe Position(LineIndex(1), ColumnIndex(500))
    }

    it("should handle adding line begins at same index (should overwrite)") {
      // While unlikely in normal usage, it's good to know behavior.
      // TreeMap.updated will overwrite.
      val lm = LineMap.create()
        .addLineBegin(CharIndex(10)) // Line 1 starts at 10
        .addLineBegin(CharIndex(10)) // Line 2 starts at 10? Actually currentLine incremented twice.

      // First call: currentLine=0 -> 1, TreeMap(0->0, 10->1)
      // Second call: currentLine=1 -> 2, TreeMap(0->0, 10->2)

      lm.indexToPosition(CharIndex(10)) shouldBe Position(LineIndex(2), ColumnIndex(0))
    }

    it("should handle indices before the first explicitly added line begin") {
      val lm = LineMap.create().addLineBegin(CharIndex(5))
      lm.indexToPosition(CharIndex(0)) shouldBe Position(LineIndex(0), ColumnIndex(0))
      lm.indexToPosition(CharIndex(4)) shouldBe Position(LineIndex(0), ColumnIndex(4))
      lm.indexToPosition(CharIndex(5)) shouldBe Position(LineIndex(1), ColumnIndex(0))
    }
  }

  describe("LineMapBuilder") {
    it("should build a LineMap correctly") {
      val builder = LineMap.create().builder
      builder.addLineBegin(CharIndex(10))
      builder.addLineBegin(CharIndex(20))

      val lm = builder.result
      lm.indexToPosition(CharIndex(0)) shouldBe Position(LineIndex(0), ColumnIndex(0))
      lm.indexToPosition(CharIndex(10)) shouldBe Position(LineIndex(1), ColumnIndex(0))
      lm.indexToPosition(CharIndex(20)) shouldBe Position(LineIndex(2), ColumnIndex(0))
    }

    it("should start from an existing LineMap") {
      val initial = LineMap.create().addLineBegin(CharIndex(10))
      val builder = LineMapBuilder.create(initial)
      builder.addLineBegin(CharIndex(20))

      val lm = builder.result
      lm.indexToPosition(CharIndex(10)) shouldBe Position(LineIndex(1), ColumnIndex(0))
      lm.indexToPosition(CharIndex(20)) shouldBe Position(LineIndex(2), ColumnIndex(0))
    }
  }
}

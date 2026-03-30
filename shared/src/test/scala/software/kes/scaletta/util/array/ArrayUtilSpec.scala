package software.kes.scaletta.util.array

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ArrayUtilSpec extends AnyFunSpec with Matchers {
  describe("ArrayUtil.growArray") {
    it("should grow an array of Strings") {
      val initial = Array("a", "b")
      val grown = ArrayUtil.growArray(initial, 3, 2)
      grown.length should be >= 3
      grown(0) shouldBe "a"
      grown(1) shouldBe "b"
      grown(2) shouldBe null
    }

    it("should not grow if capacity is sufficient") {
      val initial = Array("a", "b", "c")
      val result = ArrayUtil.growArray(initial, 2, 2)
      result should be theSameInstanceAs initial
    }

    it("should double capacity if minCapacity is not larger than double") {
      val initial = Array("a", "b")
      val grown = ArrayUtil.growArray(initial, 3, 2)
      grown.length shouldBe 4
    }

    it("should grow to minCapacity if it's larger than double") {
      val initial = Array("a", "b")
      val grown = ArrayUtil.growArray(initial, 10, 2)
      grown.length shouldBe 10
    }

    it("should not grow if minCapacity is equal to current length") {
      val initial = Array("a", "b")
      val result = ArrayUtil.growArray(initial, 2, 2)
      result should be theSameInstanceAs initial
    }

    it("should respect currentSize when copying elements") {
      val initial = Array("a", "b", "c")
      val grown = ArrayUtil.growArray(initial, 5, 1)
      grown.length shouldBe 6
      grown(0) shouldBe "a"
      grown(1) shouldBe null
      grown(2) shouldBe null
    }

    it("should handle growing from zero capacity") {
      val initial = new Array[String](0)
      val grown = ArrayUtil.growArray(initial, 1, 0)
      grown.length shouldBe 1
    }

    it("should grow an array of Ints (via generic method)") {
      val initial = Array(41, 43)
      val grown = ArrayUtil.growArray(initial, 3, 2)
      grown.length should be >= 3
      grown(0) shouldBe 41
      grown(1) shouldBe 43
    }
  }

  describe("ArrayUtil primitive grow methods") {
    it("growIntArray should grow correctly") {
      val initial = Array(1, 2)
      val grown = ArrayUtil.growIntArray(initial, 3, 2)
      grown.length shouldBe 4
      grown(0) shouldBe 1
      grown(1) shouldBe 2
    }

    it("growLongArray should grow correctly") {
      val initial = Array(1L, 2L)
      val grown = ArrayUtil.growLongArray(initial, 3, 2)
      grown.length shouldBe 4
      grown(0) shouldBe 1L
      grown(1) shouldBe 2L
    }

    it("growCharArray should grow correctly") {
      val initial = Array('a', 'b')
      val grown = ArrayUtil.growCharArray(initial, 3, 2)
      grown.length shouldBe 4
      grown(0) shouldBe 'a'
      grown(1) shouldBe 'b'
    }

    it("growByteArray should grow correctly") {
      val initial = Array(1.toByte, 2.toByte)
      val grown = ArrayUtil.growByteArray(initial, 3, 2)
      grown.length shouldBe 4
      grown(0) shouldBe 1.toByte
      grown(1) shouldBe 2.toByte
    }

    it("growShortArray should grow correctly") {
      val initial = Array(1.toShort, 2.toShort)
      val grown = ArrayUtil.growShortArray(initial, 3, 2)
      grown.length shouldBe 4
      grown(0) shouldBe 1.toShort
      grown(1) shouldBe 2.toShort
    }

    it("growFloatArray should grow correctly") {
      val initial = Array(1.0f, 2.0f)
      val grown = ArrayUtil.growFloatArray(initial, 3, 2)
      grown.length shouldBe 4
      grown(0) shouldBe 1.0f
      grown(1) shouldBe 2.0f
    }

    it("growDoubleArray should grow correctly") {
      val initial = Array(1.0, 2.0)
      val grown = ArrayUtil.growDoubleArray(initial, 3, 2)
      grown.length shouldBe 4
      grown(0) shouldBe 1.0
      grown(1) shouldBe 2.0
    }

    it("growBooleanArray should grow correctly") {
      val initial = Array(true, false)
      val grown = ArrayUtil.growBooleanArray(initial, 3, 2)
      grown.length shouldBe 4
      grown(0) shouldBe true
      grown(1) shouldBe false
    }

    it("primitive methods should handle growing from zero capacity") {
      val initial = new Array[Int](0)
      val grown = ArrayUtil.growIntArray(initial, 1, 0)
      grown.length shouldBe 1
    }

    it("primitive methods should not grow if minCapacity is equal to current length") {
      val initial = Array(1, 2)
      val result = ArrayUtil.growIntArray(initial, 2, 2)
      result should be theSameInstanceAs initial
    }

    it("primitive methods should respect currentSize when copying elements") {
      val initial = Array(1, 2, 3)
      val grown = ArrayUtil.growIntArray(initial, 5, 1)
      grown.length shouldBe 6
      grown(0) shouldBe 1
      grown(1) shouldBe 0
    }
  }
}

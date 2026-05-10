package software.kes.scaletta.api

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class LineIndexSpec extends AnyFunSuite with Matchers {
  test("LineIndex.apply should create a LineIndex for values >= 1") {
    LineIndex(1).value shouldBe 1
    LineIndex(41).value shouldBe 41
    LineIndex(101).value shouldBe 101
  }

  test("LineIndex.apply should clamp values < 1 to 1") {
    LineIndex(0).value shouldBe 1
    LineIndex(-1).value shouldBe 1
    LineIndex(Int.MinValue).value shouldBe 1
  }

  test("line(n) helper should behave like LineIndex(n)") {
    line(43).value shouldBe 43
    line(0).value shouldBe 1
  }

  test("LineIndex.next should return the next LineIndex") {
    LineIndex(1).next.value shouldBe 2
    LineIndex(43).next.value shouldBe 44
  }

  test("LineIndex.toString should return the string representation of the value") {
    LineIndex(41).toString shouldBe "41"
  }
}

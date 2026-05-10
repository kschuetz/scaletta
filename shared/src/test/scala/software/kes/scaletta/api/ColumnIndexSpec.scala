package software.kes.scaletta.api

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ColumnIndexSpec extends AnyFunSuite with Matchers {
  test("ColumnIndex.apply should create a ColumnIndex for values >= 1") {
    ColumnIndex(1).value shouldBe 1
    ColumnIndex(41).value shouldBe 41
    ColumnIndex(101).value shouldBe 101
  }

  test("ColumnIndex.apply should clamp values < 1 to 1") {
    ColumnIndex(0).value shouldBe 1
    ColumnIndex(-1).value shouldBe 1
    ColumnIndex(Int.MinValue).value shouldBe 1
  }

  test("column(n) helper should behave like ColumnIndex(n)") {
    column(43).value shouldBe 43
    column(0).value shouldBe 1
  }

  test("ColumnIndex + Int should return a new ColumnIndex") {
    (ColumnIndex(1) + 10).value shouldBe 11
    (ColumnIndex(41) + 2).value shouldBe 43
    (ColumnIndex(11) + -5).value shouldBe 6
  }

  test("ColumnIndex + Int should clamp to 1 if result is < 1") {
    (ColumnIndex(5) + -10).value shouldBe 1
  }

  test("ColumnIndex.toString should return the string representation of the value") {
    ColumnIndex(41).toString shouldBe "41"
  }
}

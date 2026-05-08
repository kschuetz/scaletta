package software.kes.scaletta.internal.reporting

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api.Position

class PosSpec extends AnyFunSpec with Matchers {
  describe("Pos") {
    it("should resolve start and end positions using an implicit LineMap") {
      implicit val lineMap: LineMap = LineMap.create()
        .addLineBegin(CharIndex(10)) // Line 2 starts at index 10
        .addLineBegin(CharIndex(20)) // Line 3 starts at index 20

      val pos = Pos("test", CharIndex(5), CharIndex(15))

      // CharIndex(5) is on Line 1, Column 6
      pos.toPosition shouldBe Position.of(1, 6)

      // CharIndex(15) is on Line 2, Column 6
      pos.toEndPosition shouldBe Position.of(2, 6)
    }

    it("should resolve positions correctly when spanning multiple lines") {
      implicit val lineMap: LineMap = LineMap.create()
        .addLineBegin(CharIndex(10))
        .addLineBegin(CharIndex(20))

      val pos = Pos("multi-line", CharIndex(0), CharIndex(25))

      pos.toPosition shouldBe Position.of(1, 1)
      pos.toEndPosition shouldBe Position.of(3, 6)
    }

    it("should work with different implicit LineMaps") {
      val lm1 = LineMap.create()
      val lm2 = LineMap.create().addLineBegin(CharIndex(5))

      val pos = Pos("x", CharIndex(10), CharIndex(10))

      locally {
        implicit val implicitLm: LineMap = lm1
        pos.toPosition shouldBe Position.of(1, 11)
      }

      locally {
        implicit val implicitLm: LineMap = lm2
        pos.toPosition shouldBe Position.of(2, 6)
      }
    }
  }
}

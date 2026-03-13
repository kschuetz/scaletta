package software.kes.scaletta.testsupport

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.testsupport.LineEndingInterpolators._

class LineEndingInterpolatorsSpec extends AnyFunSpec with Matchers {
  describe("LineEndingInterpolators") {
    describe("lf") {
      it("should convert CRLF to LF") {
        lf"line1\r\nline2" shouldBe "line1\nline2"
      }

      it("should convert CR to LF") {
        lf"line1\rline2" shouldBe "line1\nline2"
      }

      it("should keep LF as LF") {
        lf"line1\nline2" shouldBe "line1\nline2"
      }

      it("should handle mixed line endings") {
        lf"line1\r\nline2\rline3\nline4" shouldBe "line1\nline2\nline3\nline4"
      }

      it("should handle multi-line strings") {
        lf"""line1
            |line2
            |line3""".stripMargin shouldBe "line1\nline2\nline3"
      }
    }

    describe("cr") {
      it("should convert CRLF to CR") {
        cr"line1\r\nline2" shouldBe "line1\rline2"
      }

      it("should convert LF to CR") {
        cr"line1\nline2" shouldBe "line1\rline2"
      }

      it("should keep CR as CR") {
        cr"line1\rline2" shouldBe "line1\rline2"
      }

      it("should handle mixed line endings") {
        cr"line1\r\nline2\rline3\nline4" shouldBe "line1\rline2\rline3\rline4"
      }

      it("should handle multi-line strings") {
        cr"""line1
            |line2
            |line3""".stripMargin shouldBe "line1\rline2\rline3"
      }
    }

    describe("crlf") {
      it("should convert LF to CRLF") {
        crlf"line1\nline2" shouldBe "line1\r\nline2"
      }

      it("should convert CR to CRLF") {
        crlf"line1\rline2" shouldBe "line1\r\nline2"
      }

      it("should keep CRLF as CRLF") {
        crlf"line1\r\nline2" shouldBe "line1\r\nline2"
      }

      it("should handle mixed line endings") {
        crlf"line1\r\nline2\rline3\nline4" shouldBe "line1\r\nline2\r\nline3\r\nline4"
      }

      it("should handle multi-line strings") {
        crlf"""line1
              |line2
              |line3""".stripMargin shouldBe "line1\r\nline2\r\nline3"
      }
    }

    it("should support arguments in interpolators") {
      val arg = "value"
      lf"arg: $arg\r\nnext" shouldBe s"arg: $arg\nnext"
      cr"arg: $arg\nnext" shouldBe s"arg: $arg\rnext"
      crlf"arg: $arg\nnext" shouldBe s"arg: $arg\r\nnext"
    }

    describe("stripMargin") {
      it("should handle .stripMargin on multi-line strings") {
        val expectedLf = "line1\nline2\nline3"
        val expectedCr = "line1\rline2\rline3"
        val expectedCrlf = "line1\r\nline2\r\nline3"

        lf"""|line1
             |line2
             |line3""".stripMargin shouldBe expectedLf

        cr"""|line1
             |line2
             |line3""".stripMargin shouldBe expectedCr

        crlf"""|line1
               |line2
               |line3""".stripMargin shouldBe expectedCrlf
      }
    }

    describe("Edge Cases") {
      it("should handle empty strings") {
        lf"" shouldBe ""
        cr"" shouldBe ""
        crlf"" shouldBe ""
      }

      it("should handle strings with only line endings") {
        lf"\n\r\r\n" shouldBe "\n\n\n"
        cr"\n\r\r\n" shouldBe "\r\r\r"
        crlf"\n\r\r\n" shouldBe "\r\n\r\n\r\n"
      }

      it("should handle line endings at the start and end") {
        lf"\ntext\r" shouldBe "\ntext\n"
        cr"\ntext\r" shouldBe "\rtext\r"
        crlf"\ntext\r" shouldBe "\r\ntext\r\n"
      }

      it("should handle multiple consecutive line endings") {
        lf"a\n\n\r\rb" shouldBe "a\n\n\n\nb"
        cr"a\n\n\r\rb" shouldBe "a\r\r\r\rb"
        crlf"a\n\n\r\rb" shouldBe "a\r\n\r\n\r\n\r\nb"
      }

      it("should handle \\r followed by non-newline") {
        lf"line1\rx" shouldBe "line1\nx"
        cr"line1\rx" shouldBe "line1\rx"
        crlf"line1\rx" shouldBe "line1\r\nx"
      }

      it("should handle line endings inside interpolated arguments") {
        val arg = "part1\r\npart2"
        lf"start $arg end" shouldBe "start part1\npart2 end"
        cr"start $arg end" shouldBe "start part1\rpart2 end"
        crlf"start $arg end" shouldBe "start part1\r\npart2 end"
      }
    }
  }
}

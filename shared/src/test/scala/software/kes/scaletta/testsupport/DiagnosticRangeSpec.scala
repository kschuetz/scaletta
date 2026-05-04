package software.kes.scaletta.testsupport

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.internal.parser.ParseError
import software.kes.scaletta.internal.reporting.{CharIndex, LineMap, Pos}
import software.kes.scaletta.testsupport.ParseErrorMatchers._

class DiagnosticRangeSpec extends AnyFunSpec with Matchers {

  describe("Diagnostic Range DSL") {
    val dummyError = ParseError.UnexpectedToken(software.kes.scaletta.scanner.Token.At)

    it("should allow asserting a range using spanning") {
      val errors: Vector[Pos[ParseError]] = Vector(Pos(dummyError, CharIndex(5), CharIndex(10)))
      errors should containError(dummyError spanning(5, 10))
    }

    it("should fail if the range doesn't match") {
      val errors: Vector[Pos[ParseError]] = Vector(Pos(dummyError, CharIndex(5), CharIndex(10)))
      val result = containError(dummyError spanning(5, 11)).apply(errors)
      result.matches shouldBe false
      result.failureMessage should include("spanning (5, 11)")
    }

    it("should still support at(index) for backward compatibility (ignoring end index)") {
      val errors: Vector[Pos[ParseError]] = Vector(Pos(dummyError, CharIndex(5), CharIndex(10)))
      errors should containError(dummyError at 5)
    }

    describe("range matcher for Pos") {
      it("should match a Pos range") {
        val p: Pos[String] = Pos("foo", CharIndex(5), CharIndex(10))
        p should range[String](5, 10)
      }

      it("should fail if Pos range doesn't match") {
        val p: Pos[String] = Pos("foo", CharIndex(5), CharIndex(10))
        val result = range[String](5, 11).apply(p)
        result.matches shouldBe false
        result.failureMessage should include("(5, 10) did not match expected (5, 11)")
      }
    }

    describe("matchExactlyErrors with ranges") {
      val input = "f(1, @, 2)"
      val lineMap = LineMap.create() // Empty line map is fine for these tests

      it("should match exactly with ranges") {
        val errors: Vector[Pos[ParseError]] = Vector(Pos(dummyError, CharIndex(5), CharIndex(5)))
        errors should matchExactlyErrors(input, lineMap, Vector(dummyError spanning(5, 5)))
      }

      it("should fail if range mismatches in matchExactlyErrors") {
        val errors: Vector[Pos[ParseError]] = Vector(Pos(dummyError, CharIndex(5), CharIndex(5)))
        val result = matchExactlyErrors(input, lineMap, Vector(dummyError spanning(5, 6))).apply(errors)
        result.matches shouldBe false
        result.failureMessage should include("EXPECTED: UnexpectedToken(At) at index 5 to 6")
        result.failureMessage should include("ACTUAL:   UnexpectedToken(At) at index 5 to 5")
      }
    }
  }

  describe("TestErrorFormatting.renderUnderline with ranges") {
    val input = "0123456789"
    val lineMap = LineMap.create()

    it("should render a single point underline when no end index is provided") {
      val output = TestErrorFormatting.renderUnderline(input, lineMap, 5, "TEST")
      output should include("^--- (Position(1,6)) TEST")
    }

    it("should render a multi-character underline when end index is provided") {
      val output = TestErrorFormatting.renderUnderline(input, lineMap, 5, Some(7), "TEST")
      output should include("^~~--- (Position(1,6)) TEST")
    }

    it("should handle range of length 1") {
      val output = TestErrorFormatting.renderUnderline(input, lineMap, 5, Some(5), "TEST")
      output should include("^--- (Position(1,6)) TEST")
    }
  }
}

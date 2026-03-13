package software.kes.scaletta.scanner

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.testsupport.TestReaderFactory

class EscapeSequenceSpec extends AnyFunSpec with Matchers {
  describe("EscapeSequence") {
    it("should scan standard escape sequences") {
      testScan("n", EscapeResult.Success('\n'))
      testScan("t", EscapeResult.Success('\t'))
      testScan("b", EscapeResult.Success('\b'))
      testScan("r", EscapeResult.Success('\r'))
      testScan("f", EscapeResult.Success('\f'))
      testScan("\\", EscapeResult.Success('\\'))
      testScan("\"", EscapeResult.Success('"'))
      testScan("'", EscapeResult.Success('\''))
    }

    it("should scan valid Unicode escape sequences") {
      testScan("u0000", EscapeResult.Success('\u0000'))
      testScan("u0041", EscapeResult.Success('A'))
      testScan("uFFFF", EscapeResult.Success('\uFFFF'))
    }

    it("should return Boundary and backtrack when hitting a newline or carriage return") {
      testScan("\n", EscapeResult.Boundary, expectedRemainder = "\n")
      testScan("\r", EscapeResult.Boundary, expectedRemainder = "\r")
    }

    it("should return Error and backtrack for invalid non-Unicode escapes") {
      testScan("z", EscapeResult.Error(ScanError.InvalidEscapeCharacter), expectedRemainder = "z")
      testScan(" ", EscapeResult.Error(ScanError.InvalidEscapeCharacter), expectedRemainder = " ")
    }

    it("should return Error and backtrack for partial Unicode escapes (missing digits)") {
      testScan("u00", EscapeResult.Error(ScanError.InvalidEscapeCharacter), expectedRemainder = "u00")
      testScan("u0", EscapeResult.Error(ScanError.InvalidEscapeCharacter), expectedRemainder = "u0")
      testScan("u", EscapeResult.Error(ScanError.InvalidEscapeCharacter), expectedRemainder = "u")
    }

    it("should return Error and backtrack for partial Unicode escapes (invalid character)") {
      testScan("u00G", EscapeResult.Error(ScanError.InvalidEscapeCharacter), expectedRemainder = "u00G")
    }

    it("should return Boundary and backtrack when hitting a newline inside a Unicode escape") {
      testScan("u00\n", EscapeResult.Boundary, expectedRemainder = "u00\n")
    }

    it("should return Error for empty input") {
      TestReaderFactory.fromString("") { (reader, lineMap) =>
        EscapeSequence.scan(reader) shouldBe EscapeResult.Error(ScanError.InvalidEscapeCharacter)
      }
    }
  }

  private def testScan(input: String, expectedResult: EscapeResult, expectedRemainder: String = ""): Unit = {
    TestReaderFactory.fromString(input) { (reader, lineMap) =>
      val result = EscapeSequence.scan(reader)
      val actualRemainder = Iterator.continually(reader.get()).takeWhile(_.isDefined).map(_.get).mkString
      withClue(
        s"""|
            |Input: \\$input
            |Expected Result: $expectedResult
            |Actual Result:   $result
            |Expected Remainder after scan: "${expectedRemainder.getBytes.toVector}"
            |Actual Remainder after scan:   "${actualRemainder.getBytes.toVector}"
            |""".stripMargin
      ) {
        result shouldBe expectedResult
        actualRemainder shouldBe expectedRemainder
      }
    }
  }
}

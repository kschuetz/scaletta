package software.kes.scaletta.scanner

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.reporting.{CharIndex, ColumnIndex, LineIndex, Position}
import software.kes.scaletta.testsupport.TestReaderFactory

class WhitespaceTest extends AnyFunSpec with Matchers {
  describe("Whitespace.scanWhitespace") {
    it("should handle LF (\\n) as a newline") {
      val input = "  \n  "
      TestReaderFactory.fromString(input) { (reader, lineMap) =>
        val result = Whitespace.scanWhitespace(reader)
        result shouldBe WhitespaceResult.Newlines(CharIndex(2), moreThanOne = false)
        reader.currentIndex.value shouldBe 5

        // Check LineMap
        val lineMap = reader.lineMap
        lineMap.indexToPosition(CharIndex(0)) shouldBe Position(LineIndex(1), ColumnIndex(1))
        lineMap.indexToPosition(CharIndex(2)) shouldBe Position(LineIndex(1), ColumnIndex(3))
        lineMap.indexToPosition(CharIndex(3)) shouldBe Position(LineIndex(2), ColumnIndex(1))
      }
    }

    it("should handle CRLF (\\r\\n) as a single logical newline") {
      val input = "  \r\n  "
      TestReaderFactory.fromString(input) { (reader, lineMap) =>
        val result = Whitespace.scanWhitespace(reader)
        // \r is at index 2, \n is at index 3.
        // WhitespaceResult.Newlines.lastIndex should be index of \r (start of sequence)
        result shouldBe WhitespaceResult.Newlines(CharIndex(2), moreThanOne = false)
        reader.currentIndex.value shouldBe 6

        // Check LineMap - next line should start at index 4 (after \n)
        val lineMap = reader.lineMap
        lineMap.indexToPosition(CharIndex(3)) shouldBe Position(LineIndex(1), ColumnIndex(4))
        lineMap.indexToPosition(CharIndex(4)) shouldBe Position(LineIndex(2), ColumnIndex(1))
      }
    }

    it("should handle lone CR (\\r) as a logical newline") {
      val input = "  \r \r "
      TestReaderFactory.fromString(input) { (reader, lineMap) =>
        val result = Whitespace.scanWhitespace(reader)
        result shouldBe WhitespaceResult.Newlines(CharIndex(4), moreThanOne = true)
        reader.currentIndex.value shouldBe 6

        // Check LineMap
        val lineMap = reader.lineMap
        // First \r at index 2, next line starts at 3
        lineMap.indexToPosition(CharIndex(3)).line.value shouldBe 2
        // Second \r at index 4, next line starts at 5
        lineMap.indexToPosition(CharIndex(5)).line.value shouldBe 3
      }
    }

    it("should detect more than one newline") {
      val input = "\n\n"
      TestReaderFactory.fromString(input) { (reader, lineMap) =>
        val result = Whitespace.scanWhitespace(reader)
        result shouldBe WhitespaceResult.Newlines(CharIndex(1), moreThanOne = true)
      }
    }

    it("should detect more than one newline with mixed endings") {
      val input = "\r\n\r"
      TestReaderFactory.fromString(input) { (reader, lineMap) =>
        val result = Whitespace.scanWhitespace(reader)
        result shouldBe WhitespaceResult.Newlines(CharIndex(2), moreThanOne = true)
      }
    }

    it("should handle horizontal whitespace without newlines") {
      val input = " \t "
      TestReaderFactory.fromString(input) { (reader, lineMap) =>
        val result = Whitespace.scanWhitespace(reader)
        result shouldBe WhitespaceResult.NoNewlines
        reader.currentIndex.value shouldBe 3
      }
    }

    it("should return NoWhitespace for empty input") {
      TestReaderFactory.fromString("") { (reader, lineMap) =>
        val result = Whitespace.scanWhitespace(reader)
        result shouldBe WhitespaceResult.NoWhitespace
      }
    }

    it("should stop at non-whitespace and unget it") {
      val input = "  x"
      TestReaderFactory.fromString(input) { (reader, lineMap) =>
        val result = Whitespace.scanWhitespace(reader)
        result shouldBe WhitespaceResult.NoNewlines
        reader.currentIndex.value shouldBe 2
        reader.get() shouldBe Some('x')
      }
    }
  }
}

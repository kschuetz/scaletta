package software.kes.scaletta.internal.reader

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.reporting.Position
import software.kes.scaletta.testsupport.TestReaderFactory

class SourceReaderSpec extends AnyFunSpec with Matchers {
  describe("CRLF Handling") {
    it("should maintain correct character indices when encountering CRLF") {
      // The input has a Windows-style line ending (CRLF) between 'a' and 'b'.
      // Indices:
      // 'a' : 0
      // '\r': 1
      // '\n': 2
      // 'b' : 3
      val input = "a\r\nb"
      TestReaderFactory.fromString(input) { (reader, lineMap) =>
        // Read 'a'
        reader.get() shouldBe Some('a')
        reader.prevIndex.value shouldBe 0

        // Read '\r'
        reader.get() shouldBe Some('\r')
        reader.prevIndex.value shouldBe 1

        // Read '\n'
        reader.get() shouldBe Some('\n')
        reader.prevIndex.value shouldBe 2

        // Read 'b'
        reader.get() shouldBe Some('b')
        reader.prevIndex.value shouldBe 3
      }
    }

    it("should maintain correct indices after unget of CRLF") {
      val input = "a\r\nb"
      TestReaderFactory.fromString(input) { (reader, lineMap) =>
        reader.get() shouldBe Some('a') // index 0
        reader.get() shouldBe Some('\r') // index 1
        reader.get() shouldBe Some('\n') // index 2

        reader.unget('\n')
        reader.currentIndex.value shouldBe 2

        reader.unget('\r')
        reader.currentIndex.value shouldBe 1

        reader.get() shouldBe Some('\r')
        reader.get() shouldBe Some('\n')
        reader.get() shouldBe Some('b')
        reader.prevIndex.value shouldBe 3
      }
    }

    it("should NOT normalize CRLF") {
      val input = "a\r\nb"
      TestReaderFactory.fromString(input) { (reader, lineMap) =>
        reader.get() shouldBe Some('a') // 0
        reader.get() shouldBe Some('\r') // 1
        reader.get() shouldBe Some('\n') // 2
        reader.get() shouldBe Some('b') // 3
      }
    }

    it("should correctly handle peek") {
      val input = "\r\n"
      TestReaderFactory.fromString(input) { (reader, lineMap) =>
        reader.peek() shouldBe Some('\r')
        reader.get() shouldBe Some('\r')
        reader.peek() shouldBe Some('\n')
        reader.get() shouldBe Some('\n')
      }
    }

    it("should correctly handle peek interactions without normalization") {
      val input = "\r\n"
      TestReaderFactory.fromString(input) { (reader, lineMap) =>
        reader.peek() shouldBe Some('\r')
        reader.currentIndex.value shouldBe 0 // peek should not advance index
        reader.get() shouldBe Some('\r')
        reader.currentIndex.value shouldBe 1
      }
    }
  }

  describe("core methods") {
    it("should correctly handle tryGet") {
      val input = "abc"
      TestReaderFactory.fromString(input) { (reader, lineMap) =>
        reader.tryGet('a') shouldBe true
        reader.tryGet('x') shouldBe false
        reader.get() shouldBe Some('b')
        reader.tryGet('c') shouldBe true
        reader.get() shouldBe None
      }
    }

    it("should correctly handle matchSequence") {
      val input = "abcdef"
      TestReaderFactory.fromString(input) { (reader, lineMap) =>
        reader.matchSequence("abc") shouldBe true
        reader.currentIndex.value shouldBe 3

        // Should restore state on failure
        reader.matchSequence("xyz") shouldBe false
        reader.currentIndex.value shouldBe 3
        reader.get() shouldBe Some('d')
      }
    }

    it("should correctly handle skipWhile") {
      val input = "   abc"
      TestReaderFactory.fromString(input) { (reader, lineMap) =>
        reader.skipWhile(_.isWhitespace)
        reader.get() shouldBe Some('a')
      }
    }

    it("should correctly handle skipUntil") {
      val input = "abc   def"
      TestReaderFactory.fromString(input) { (reader, lineMap) =>
        reader.skipUntil(_.isWhitespace)
        reader.currentIndex.value shouldBe 3
        reader.get() shouldBe Some(' ')
      }
    }

    it("should correctly handle ungetString") {
      val input = "abc"
      TestReaderFactory.fromString(input) { (reader, lineMap) =>
        reader.get() shouldBe Some('a')
        reader.get() shouldBe Some('b')
        reader.ungetString("ab")
        reader.currentIndex.value shouldBe 0
        reader.get() shouldBe Some('a')
        reader.get() shouldBe Some('b')
      }
    }
  }

  describe("edge cases and invariants") {
    it("should handle empty input") {
      TestReaderFactory.fromString("") { (reader, lineMap) =>
        reader.get() shouldBe None
        reader.peek() shouldBe None
        reader.tryGet('a') shouldBe false
        reader.matchSequence("abc") shouldBe false
      }
    }

    it("should handle multiple unget calls correctly") {
      val input = "a\r\nb"
      TestReaderFactory.fromString(input) { (reader, lineMap) =>
        reader.get() shouldBe Some('a') // index 0 -> 1
        reader.get() shouldBe Some('\r') // index 1 -> 2
        reader.get() shouldBe Some('\n') // index 2 -> 3
        reader.get() shouldBe Some('b') // index 3 -> 4

        reader.unget('b') // index 4 -> 3
        reader.currentIndex.value shouldBe 3
        reader.unget('\n') // index 3 -> 2
        reader.currentIndex.value shouldBe 2
        reader.unget('\r') // index 2 -> 1
        reader.currentIndex.value shouldBe 1
      }
    }

    it("should correctly track line and column positions") {
      val input = "line1\nline2\r\nline3"
      TestReaderFactory.fromString(input) { (reader, _) =>
        reader.lineMap.indexToPosition(reader.currentIndex) shouldBe Position.of(1, 1)

        // Consume "line1\n"
        "line1\n".foreach(_ => reader.get())
        reader.currentIndex.value shouldBe 6
        reader.lineMap.indexToPosition(reader.currentIndex) shouldBe Position.of(2, 1)

        // Consume "line2\r\n"
        "line2\r\n".foreach(_ => reader.get())
        reader.currentIndex.value shouldBe 13
        reader.lineMap.indexToPosition(reader.currentIndex) shouldBe Position.of(3, 1)

        // Go back across CRLF
        reader.unget('\n')
        reader.unget('\r')
        reader.currentIndex.value shouldBe 11
        reader.lineMap.indexToPosition(reader.currentIndex) shouldBe Position.of(2, 6)

        // Read CRLF again
        reader.get() shouldBe Some('\r')
        reader.get() shouldBe Some('\n')
        reader.lineMap.indexToPosition(reader.currentIndex) shouldBe Position.of(3, 1)
      }
    }

    it("should support nested settings") {
      TestReaderFactory.fromString("") { (reader, lineMap) =>
        val s = reader.settings
        reader.pushSettings(identity)
        reader.settings shouldBe s
        reader.popSettings()
        reader.settings shouldBe s
      }
    }

    it("should correctly handle peek followed by unget interaction") {
      val input = "abc"
      TestReaderFactory.fromString(input) { (reader, lineMap) =>
        reader.get() shouldBe Some('a')
        reader.peek() shouldBe Some('b')
        reader.unget('a')
        reader.get() shouldBe Some('a')
        reader.get() shouldBe Some('b')
      }
    }
  }
}

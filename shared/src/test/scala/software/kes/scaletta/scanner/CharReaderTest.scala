package software.kes.scaletta.scanner

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.testsupport.TestReaderFactory

class CharReaderTest extends AnyFunSpec with Matchers {
  describe("CRLF Handling") {
    it("should maintain correct character indices when encountering CRLF") {
      // The input has a Windows-style line ending (CRLF) between 'a' and 'b'.
      // Indices:
      // 'a' : 0
      // '\r': 1
      // '\n': 2
      // 'b' : 3
      val input = "a\r\nb"
      TestReaderFactory.fromString(input) { reader =>
        // Read 'a'
        reader.get() shouldBe Some('a')
        reader.prevIndex.value shouldBe 0

        // Read the newline (normalized from CRLF)
        reader.get() shouldBe Some('\n')
        // Here is the expectation:
        // If we treat CRLF as a single "newline" event, it should have consumed up to index 2.
        // The NEXT character 'b' should be at index 3.

        reader.get() shouldBe Some('b')
        // Fails in current implementation: 'b' will likely be at index 2 because
        // the current CharReader consumes '\r', sees it, peek/gets '\n', then 
        // ungets the '\n' or pushes it back, only incrementing index by 1 for the '\r'.
        reader.prevIndex.value shouldBe 3
      }
    }

    it("should maintain correct indices after unget of CRLF-normalized newline") {
      val input = "a\r\nb"
      TestReaderFactory.fromString(input) { reader =>
        reader.get() shouldBe Some('a') // index 0
        val newline = reader.get() // normalized from \r\n (indices 1, 2)
        newline shouldBe Some('\n')

        reader.unget('\n')
        // After unget, we should be back at the start of the CRLF (index 1)
        reader.currentIndex.value shouldBe 1

        reader.get() shouldBe Some('\n')
        reader.get() shouldBe Some('b')
        reader.prevIndex.value shouldBe 3
      }
    }

    it("should NOT normalize CRLF when normalization is disabled") {
      val input = "a\r\nb"
      TestReaderFactory.fromString(input) { reader =>
        reader.modifySettings(_.copy(normalizeNewLines = false))

        reader.get() shouldBe Some('a') // 0
        reader.get() shouldBe Some('\r') // 1
        reader.get() shouldBe Some('\n') // 2
        reader.get() shouldBe Some('b') // 3
      }
    }

    it("should correctly handle peek with normalization disabled") {
      val input = "\r\n"
      TestReaderFactory.fromString(input) { reader =>
        reader.modifySettings(_.copy(normalizeNewLines = false))
        reader.peek() shouldBe Some('\r')
        reader.get() shouldBe Some('\r')
        reader.peek() shouldBe Some('\n')
        reader.get() shouldBe Some('\n')
      }
    }

    it("should toggle normalization correctly") {
      val input = "\r\n\r\n\r"
      TestReaderFactory.fromString(input) { reader =>
        reader.modifySettings(_.copy(normalizeNewLines = false))
        reader.get() shouldBe Some('\r')
        reader.get() shouldBe Some('\n')

        reader.modifySettings(_.copy(normalizeNewLines = true))
        reader.get() shouldBe Some('\n') // normalized from \r\n
        reader.get() shouldBe Some('\n') // normalized from \r
        reader.get() shouldBe None
      }
    }

    it("should support pushSettings and popSettings") {
      val input = "\r\n\r\n"
      TestReaderFactory.fromString(input) { reader =>
        reader.settings.normalizeNewLines shouldBe true

        reader.pushSettings(_.copy(normalizeNewLines = false))
        reader.settings.normalizeNewLines shouldBe false
        reader.get() shouldBe Some('\r')
        reader.get() shouldBe Some('\n')

        reader.popSettings()
        reader.settings.normalizeNewLines shouldBe true
        reader.get() shouldBe Some('\n') // normalized from \r\n
        reader.get() shouldBe None
      }
    }

    it("should correctly handle unget of a CR-only normalized newline") {
      val input = "a\rx"
      TestReaderFactory.fromString(input) { reader =>
        reader.get() shouldBe Some('a') // index 0
        reader.get() shouldBe Some('\n') // normalized from \r (index 1)

        reader.unget('\n')
        reader.currentIndex.value shouldBe 1 // should be back at index 1 (\r)

        reader.get() shouldBe Some('\n') // normalized from \r (index 1)
        reader.get() shouldBe Some('x') // index 2
        reader.prevIndex.value shouldBe 2
      }
    }

    it("should report correct positions for CR-only newlines") {
      val input = "a\rb"
      TestReaderFactory.fromString(input) { reader =>
        reader.get() shouldBe Some('a') // line 0, col 0
        reader.get() shouldBe Some('\n') // normalized \r (line 0, col 1)

        val posB = reader.lineMap.indexToPosition(reader.currentIndex)
        posB.line.value shouldBe 1
        posB.column.value shouldBe 0

        reader.get() shouldBe Some('b') // line 1, col 0
      }
    }

    it("should normalize a trailing CR at EOF") {
      val input = "a\r"
      TestReaderFactory.fromString(input) { reader =>
        reader.get() shouldBe Some('a')
        reader.get() shouldBe Some('\n') // normalized from trailing \r
        reader.get() shouldBe None
      }
    }

    it("should handle consecutive CR characters correctly") {
      val input = "\r\r"
      TestReaderFactory.fromString(input) { reader =>
        reader.get() shouldBe Some('\n')
        reader.get() shouldBe Some('\n')
        reader.get() shouldBe None
      }
    }
  }

  describe("core methods") {
    it("should correctly handle tryGet") {
      val input = "abc"
      TestReaderFactory.fromString(input) { reader =>
        reader.tryGet('a') shouldBe true
        reader.tryGet('x') shouldBe false
        reader.get() shouldBe Some('b')
        reader.tryGet('c') shouldBe true
        reader.get() shouldBe None
      }
    }

    it("should correctly handle matchSequence") {
      val input = "abcdef"
      TestReaderFactory.fromString(input) { reader =>
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
      TestReaderFactory.fromString(input) { reader =>
        reader.skipWhile(_.isWhitespace)
        reader.get() shouldBe Some('a')
      }
    }

    it("should correctly handle skipUntil") {
      val input = "abc   def"
      TestReaderFactory.fromString(input) { reader =>
        reader.skipUntil(_.isWhitespace)
        reader.currentIndex.value shouldBe 3
        reader.get() shouldBe Some(' ')
      }
    }

    it("should correctly handle ungetString") {
      val input = "abc"
      TestReaderFactory.fromString(input) { reader =>
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
      TestReaderFactory.fromString("") { reader =>
        reader.get() shouldBe None
        reader.peek() shouldBe None
        reader.tryGet('a') shouldBe false
        reader.matchSequence("abc") shouldBe false
      }
    }

    // TODO: fix this test
    ignore("should handle multiple unget calls correctly") {
      // Current implementation of unget uses lastReadWidth.
      // Consecutive unget calls (without an intervening get) might be problematic
      // if the widths differ.
      val input = "a\r\nb"
      TestReaderFactory.fromString(input) { reader =>
        reader.get() shouldBe Some('a') // width 1, index 0 -> 1
        reader.get() shouldBe Some('\n') // width 2, index 1 -> 3
        reader.get() shouldBe Some('b') // width 1, index 3 -> 4

        reader.unget('b') // width 1, index 4 -> 3
        reader.currentIndex.value shouldBe 3
        reader.unget('\n') // width 2, index 3 -> 1
        reader.currentIndex.value shouldBe 1
      }
    }

    it("should correctly manage LineMap highWater mark when re-reading lines") {
      val input = "line1\nline2"
      TestReaderFactory.fromString(input) { reader =>
        // reader.get() shouldBe Some('l') // Removed to avoid offset
        reader.lineMap.indexToPosition(CharIndex(0)).line.value shouldBe 0

        // Consume "line1\n"
        "line1\n".foreach { ch =>
          reader.get() shouldBe Some(ch)
        }
        reader.currentIndex.value shouldBe 6 // after \n
        reader.lineMap.indexToPosition(CharIndex(6)).line.value shouldBe 1

        // unget across newline
        reader.unget('\n')
        reader.currentIndex.value shouldBe 5

        // Read it again. recordNewline should NOT add a duplicate entry if highWater works.
        reader.get() shouldBe Some('\n')
        reader.currentIndex.value shouldBe 6

        // Check if LineMap has correct number of entries (implicit check by looking at positions)
        reader.lineMap.indexToPosition(CharIndex(6)).line.value shouldBe 1
      }
    }

    it("should support nested settings") {
      TestReaderFactory.fromString("") { reader =>
        reader.settings.normalizeNewLines shouldBe true
        reader.pushSettings(_.copy(normalizeNewLines = false))
        reader.settings.normalizeNewLines shouldBe false
        reader.pushSettings(_.copy(normalizeNewLines = true))
        reader.settings.normalizeNewLines shouldBe true
        reader.popSettings()
        reader.settings.normalizeNewLines shouldBe false
        reader.popSettings()
        reader.settings.normalizeNewLines shouldBe true
      }
    }

    it("should correctly handle peek interactions with normalization") {
      val input = "\r\n"
      TestReaderFactory.fromString(input) { reader =>
        reader.peek() shouldBe Some('\n')
        reader.currentIndex.value shouldBe 0 // peek should not advance index
        reader.get() shouldBe Some('\n')
        reader.currentIndex.value shouldBe 2 // should have advanced by 2
      }
    }

    it("should correctly handle peek followed by unget interaction") {
      val input = "abc"
      TestReaderFactory.fromString(input) { reader =>
        reader.get() shouldBe Some('a')
        reader.peek() shouldBe Some('b')
        reader.unget('a')
        reader.get() shouldBe Some('a')
        reader.get() shouldBe Some('b')
      }
    }
  }
}

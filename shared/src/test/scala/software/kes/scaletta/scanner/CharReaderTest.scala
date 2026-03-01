package software.kes.scaletta.scanner

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.testsupport.TestReaderFactory

class CharReaderTest extends AnyFunSpec with Matchers {
  describe("CharReader CRLF Handling") {
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
}

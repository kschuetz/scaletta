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
  }
}

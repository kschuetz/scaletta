package software.kes.scaletta.internal.scanner

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.testsupport.TestReaderFactory

class UnionIntersectionScannerSpec extends AnyFunSpec with Matchers {
  describe("Scanner") {
    it("should scan '|' as Token.Pipe") {
      val input = "|"
      TestReaderFactory.fromString(input) { (reader, _) =>
        val scanner = Scanner.create(reader, IdentifierPolicy.Default)
        scanner.get().value shouldBe Token.Pipe
      }
    }

    it("should scan '&' as Token.Ampersand") {
      val input = "&"
      TestReaderFactory.fromString(input) { (reader, _) =>
        val scanner = Scanner.create(reader, IdentifierPolicy.Default)
        scanner.get().value shouldBe Token.Ampersand
      }
    }

    it("should scan '|' and '&' in type-like contexts") {
      val input = "A | B & C"
      TestReaderFactory.fromString(input) { (reader, _) =>
        val scanner = Scanner.create(reader, IdentifierPolicy.Default)
        scanner.get().value shouldBe Token.Identifier.Upper("A")
        scanner.get().value shouldBe Token.Pipe
        scanner.get().value shouldBe Token.Identifier.Upper("B")
        scanner.get().value shouldBe Token.Ampersand
        scanner.get().value shouldBe Token.Identifier.Upper("C")
      }
    }

    it("should scan longer operators containing '|' and '&'") {
      val input = "|| && |+ &+ |= &="
      TestReaderFactory.fromString(input) { (reader, _) =>
        val scanner = Scanner.create(reader, IdentifierPolicy.Default)
        scanner.get().value shouldBe Token.Identifier.Operator("||")
        scanner.get().value shouldBe Token.Identifier.Operator("&&")
        scanner.get().value shouldBe Token.Identifier.Operator("|+")
        scanner.get().value shouldBe Token.Identifier.Operator("&+")
        scanner.get().value shouldBe Token.Identifier.Operator("|=")
        scanner.get().value shouldBe Token.Identifier.Operator("&=")
      }
    }
  }
}

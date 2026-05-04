package software.kes.scaletta.internal.scanner

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.internal.reporting.{CharIndex, Pos}
import software.kes.scaletta.testsupport.ScannerTestHelpers.success
import software.kes.scaletta.testsupport.{AssertExpectedTokens, TestReaderFactory}

class IdentifierUnicodeSpec extends AnyFunSpec with Matchers with AssertExpectedTokens {
  describe("unicode identifiers") {
    it("should handle identifiers starting with Greek letters") {
      val input = "αβγ"
      TestReaderFactory.fromString(input) { (reader, lineMap) =>
        val scanner = Scanner.create(reader, IdentifierPolicy.Default)
        val tokens = Vector(scanner.get(), scanner.get())
        assertExpectedTokens(input, lineMap, Vector(
          success(Token.Identifier.Lower("αβγ"), 0, 2),
          success(Token.EndOfInput, 3, 3)
        ), tokens)
      }
    }

    it("should handle identifiers starting with Cyrillic uppercase letters") {
      val input = "Привіт"
      TestReaderFactory.fromString(input) { (reader, lineMap) =>
        val scanner = Scanner.create(reader, IdentifierPolicy.Default)
        val tokens = Vector(scanner.get(), scanner.get())
        assertExpectedTokens(input, lineMap, Vector(
          success(Token.Identifier.Upper("Привіт"), 0, 5),
          success(Token.EndOfInput, 6, 6)
        ), tokens)
      }
    }

    it("should handle identifiers with combining marks") {
      // 'a' followed by COMBINING ACUTE ACCENT (U+0301)
      val input = "a\u0301bc"
      TestReaderFactory.fromString(input) { (reader, lineMap) =>
        val scanner = Scanner.create(reader, IdentifierPolicy.Default)
        val tokens = Vector(scanner.get(), scanner.get())
        assertExpectedTokens(input, lineMap, Vector(
          success(Token.Identifier.Lower("a\u0301bc"), 0, 3),
          success(Token.EndOfInput, 4, 4)
        ), tokens)
      }
    }

    it("should handle Unicode math symbols as operators") {
      val input = "∑"
      TestReaderFactory.fromString(input) { (reader, lineMap) =>
        val scanner = Scanner.create(reader, IdentifierPolicy.Default)
        val tokens = Vector(scanner.get(), scanner.get())
        assertExpectedTokens(input, lineMap, Vector(
          success(Token.Identifier.Operator("∑"), 0, 0),
          success(Token.EndOfInput, 1, 1)
        ), tokens)
      }
    }

    it("should handle multiple Unicode operators") {
      val input = "⊕⊗"
      TestReaderFactory.fromString(input) { (reader, lineMap) =>
        val scanner = Scanner.create(reader, IdentifierPolicy.Default)
        val tokens = Vector(scanner.get(), scanner.get())
        assertExpectedTokens(input, lineMap, Vector(
          success(Token.Identifier.Operator("⊕⊗"), 0, 1),
          success(Token.EndOfInput, 2, 2)
        ), tokens)
      }
    }

    it("should handle Unicode escape sequences in quoted identifiers") {
      val input = "`\\u03BB`"
      TestReaderFactory.fromString(input) { (reader, lineMap) =>
        val scanner = Scanner.create(reader, IdentifierPolicy.Default)
        val tokens = Vector(scanner.get(), scanner.get())
        assertExpectedTokens(input, lineMap, Vector(
          success(Token.Identifier.Quoted("\u03BB"), 0, 7),
          success(Token.EndOfInput, 8, 8)
        ), tokens)
      }
    }

    it("should handle exotic characters directly in quoted identifiers") {
      val input = "`🚀`"
      TestReaderFactory.fromString(input) { (reader, lineMap) =>
        val scanner = Scanner.create(reader, IdentifierPolicy.Default)
        val tokens = Vector(scanner.get(), scanner.get())
        assertExpectedTokens(input, lineMap, Vector(
          success(Token.Identifier.Quoted("🚀"), 0, 3), // Note: 🚀 is 2 chars in UTF-16
          success(Token.EndOfInput, 4, 4)
        ), tokens)
      }
    }

    it("should handle mixed ASCII and Unicode in identifiers") {
      val input = "scala_π"
      TestReaderFactory.fromString(input) { (reader, lineMap) =>
        val scanner = Scanner.create(reader, IdentifierPolicy.Default)
        val tokens = Vector(scanner.get(), scanner.get())
        assertExpectedTokens(input, lineMap, Vector(
          success(Token.Identifier.Lower("scala_π"), 0, 6),
          success(Token.EndOfInput, 7, 7)
        ), tokens)
      }
    }

    it("should handle titlecase letters as uppercase identifiers") {
      // U+01C5 is LATIN CAPITAL LETTER D WITH SMALL LETTER Z WITH CARON (Titlecase)
      val input = "\u01C5z"
      TestReaderFactory.fromString(input) { (reader, lineMap) =>
        val scanner = Scanner.create(reader, IdentifierPolicy.Default)
        val tokens = Vector(scanner.get(), scanner.get())
        assertExpectedTokens(input, lineMap, Vector(
          success(Token.Identifier.Upper("\u01C5z"), 0, 1),
          success(Token.EndOfInput, 2, 2)
        ), tokens)
      }
    }

    it("should handle letter numbers as letters") {
      // U+2164 is ROMAN NUMERAL FIVE (Letter Number)
      val input = "\u2164"
      TestReaderFactory.fromString(input) { (reader, lineMap) =>
        val scanner = Scanner.create(reader, IdentifierPolicy.Default)
        val tokens = Vector(scanner.get(), scanner.get())
        // Roman numeral V is often treated as uppercase letter
        assertExpectedTokens(input, lineMap, Vector(
          success(Token.Identifier.Upper("\u2164"), 0, 0),
          success(Token.EndOfInput, 1, 1)
        ), tokens)
      }
    }

    it("should handle multi-unit Unicode characters in quoted identifiers (surrogate pairs)") {
      // U+1F680 (🚀) is \uD83D\uDE80
      val input = "`\uD83D\uDE80`"
      TestReaderFactory.fromString(input) { (reader, lineMap) =>
        val scanner = Scanner.create(reader, IdentifierPolicy.Default)
        val tokens = Vector(scanner.get(), scanner.get())
        assertExpectedTokens(input, lineMap, Vector(
          success(Token.Identifier.Quoted("🚀"), 0, 3),
          success(Token.EndOfInput, 4, 4)
        ), tokens)
      }
    }

    it("should handle Unicode characters above U+FFFF in identifiers if allowed by Character.isLetter") {
      // U+10400 is DESERET CAPITAL LETTER LONG I
      val input = "\uD801\uDC00"
      TestReaderFactory.fromString(input) { (reader, lineMap) =>
        val scanner = Scanner.create(reader, IdentifierPolicy.Default)
        val tokens = Vector(scanner.get(), scanner.get())
        // Current behavior: they are treated as InvalidCharacter because Character.isLetter(Char)
        // and Character.getType(Char) don't handle surrogates as single code points.
        assertExpectedTokens(input, lineMap, Vector(
          Pos(Token.Error(ScanError.InvalidCharacter), CharIndex(0), CharIndex(1)),
          success(Token.EndOfInput, 2, 2)
        ), tokens)
      }
    }
    it("should handle identifiers starting with other letters (e.g., Hebrew)") {
      // U+05D0 is HEBREW LETTER ALEPH (Other Letter)
      val input = "א"
      TestReaderFactory.fromString(input) { (reader, lineMap) =>
        val scanner = Scanner.create(reader, IdentifierPolicy.Default)
        val tokens = Vector(scanner.get(), scanner.get())
        assertExpectedTokens(input, lineMap, Vector(
          success(Token.Identifier.Lower("א"), 0, 0),
          success(Token.EndOfInput, 1, 1)
        ), tokens)
      }
    }

    it("should handle letters with modifier letters") {
      // 'a' followed by MODIFIER LETTER SMALL H (U+02B0)
      val input = "a\u02B0"
      TestReaderFactory.fromString(input) { (reader, lineMap) =>
        val scanner = Scanner.create(reader, IdentifierPolicy.Default)
        val tokens = Vector(scanner.get(), scanner.get())
        assertExpectedTokens(input, lineMap, Vector(
          success(Token.Identifier.Lower("a\u02B0"), 0, 1),
          success(Token.EndOfInput, 2, 2)
        ), tokens)
      }
    }
  }
}

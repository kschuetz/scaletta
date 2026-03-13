package software.kes.scaletta.scanner

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.common.Interpolator
import software.kes.scaletta.testsupport.{AssertExpectedTokens, TestReaderFactory}

class PortalExitSpec extends AnyFunSpec with Matchers with AssertExpectedTokens {
  describe("Scanner - portal exit boundaries") {
    it("should exit immediately at the closing brace when it's the next character") {
      val input = "}trailing"
      TestReaderFactory.fromString(input) { (reader, lineMap) =>
        val scanner = Scanner.create(reader, IdentifierPolicy.Default, portalMode = true)

        val t1 = scanner.get()
        t1.value shouldBe Token.EndOfInput
        t1.begin.value shouldBe 1
        t1.end.value shouldBe 1

        // The reader should be positioned AFTER the '}'
        reader.currentIndex.value shouldBe 1
        reader.peek() shouldBe Some('t')
      }
    }

    it("should handle portal exit after an identifier without space") {
      val input = "ident}next"
      TestReaderFactory.fromString(input) { (reader, lineMap) =>
        val scanner = Scanner.create(reader, IdentifierPolicy.Default, portalMode = true)

        val t1 = scanner.get()
        t1.value shouldBe Token.Identifier.Lower("ident")
        t1.begin.value shouldBe 0
        t1.end.value shouldBe 4

        val t2 = scanner.get()
        t2.value shouldBe Token.EndOfInput
        t2.begin.value shouldBe 6
        t2.end.value shouldBe 6

        reader.currentIndex.value shouldBe 6
        reader.peek() shouldBe Some('n')
      }
    }

    it("should handle portal exit with preceding whitespace") {
      val input = "ident  }next"
      TestReaderFactory.fromString(input) { (reader, lineMap) =>
        val scanner = Scanner.create(reader, IdentifierPolicy.Default, portalMode = true)

        val t1 = scanner.get()
        t1.value shouldBe Token.Identifier.Lower("ident")

        val t2 = scanner.get()
        t2.value shouldBe Token.EndOfInput

        // After 'ident' (0-4), there are two spaces (5, 6) and then '}' (7).
        // The EndOfInput should report the position after the '}'
        t2.begin.value shouldBe 8
        t2.end.value shouldBe 8

        reader.currentIndex.value shouldBe 8
        reader.peek() shouldBe Some('n')
      }
    }

    it("should handle portal exit with preceding newline") {
      val input = "ident\n}next"
      TestReaderFactory.fromString(input) { (reader, lineMap) =>
        val scanner = Scanner.create(reader, IdentifierPolicy.Default, portalMode = true)

        val t1 = scanner.get()
        t1.value shouldBe Token.Identifier.Lower("ident")

        // newline at index 5, '}' at index 6
        val t2 = scanner.get()
        // Note: Semicolon inference might kick in here!
        // 'ident' can terminate statement. '}' can NOT begin statement?
        // Let's check Token.canBeginStatement
        t2.value shouldBe Token.EndOfInput
        t2.begin.value shouldBe 7

        reader.currentIndex.value shouldBe 7
        reader.peek() shouldBe Some('n')
      }
    }

    it("should NOT infer semicolon before portal exit even if there is a newline") {
      // Scala doesn't infer semicolon before '}'
      val input = "1\n}"
      TestReaderFactory.fromString(input) { (reader, lineMap) =>
        val scanner = Scanner.create(reader, IdentifierPolicy.Default, portalMode = true)

        scanner.get().value shouldBe Token.IntLiteral(1)
        scanner.get().value shouldBe Token.EndOfInput
        reader.currentIndex.value shouldBe 3
      }
    }

    it("should handle nested braces correctly and only exit at the top-level portal brace") {
      val input = "{ { x } } }tail"
      TestReaderFactory.fromString(input) { (reader, lineMap) =>
        val scanner = Scanner.create(reader, IdentifierPolicy.Default, portalMode = true)

        scanner.get().value shouldBe Token.LBrace // index 0
        scanner.get().value shouldBe Token.LBrace // index 2
        scanner.get().value shouldBe Token.Identifier.Lower("x") // index 4
        scanner.get().value shouldBe Token.RBrace // index 6
        scanner.get().value shouldBe Token.RBrace // index 8

        // This '}' at index 10 should be the portal exit
        val t6 = scanner.get()
        t6.value shouldBe Token.EndOfInput
        t6.begin.value shouldBe 11
        t6.end.value shouldBe 11

        reader.currentIndex.value shouldBe 11
        reader.peek() shouldBe Some('t')
      }
    }

    it("should handle comments before portal exit") {
      val input = "x // comment\n}tail"
      TestReaderFactory.fromString(input) { (reader, lineMap) =>
        val scanner = Scanner.create(reader, IdentifierPolicy.Default, portalMode = true)

        scanner.get().value shouldBe Token.Identifier.Lower("x")

        val t2 = scanner.get()
        t2.value shouldBe Token.EndOfInput
        t2.begin.value shouldBe 14 // index after '}'

        reader.currentIndex.value shouldBe 14
        reader.peek() shouldBe Some('t')
      }
    }

    it("should handle block comments before portal exit") {
      val input = "x /* comment */ }tail"
      TestReaderFactory.fromString(input) { (reader, lineMap) =>
        val scanner = Scanner.create(reader, IdentifierPolicy.Default, portalMode = true)

        scanner.get().value shouldBe Token.Identifier.Lower("x")

        val t2 = scanner.get()
        t2.value shouldBe Token.EndOfInput
        t2.begin.value shouldBe 17 // index after '}'

        reader.currentIndex.value shouldBe 17
        reader.peek() shouldBe Some('t')
      }
    }

    it("should handle portal exit inside an interpolated string (double portal situation)") {
      // This is tricky. Template -> ${ portal -> s"${ nested portal } " }
      // The Scanner itself doesn't know about the outer template, but it handles nested portals via RegionStack.

      // Input representing: s"${ x }" followed by portal exit '}'
      val input = "s\"${ x }\" }tail"
      TestReaderFactory.fromString(input) { (reader, lineMap) =>
        val scanner = Scanner.create(reader, IdentifierPolicy.Default, portalMode = true)

        scanner.get().value shouldBe Token.BeginInterpolatedString(Interpolator.Custom("s"))
        scanner.get().value shouldBe Token.BeginInterpolatedEscape // ${
        scanner.get().value shouldBe Token.Identifier.Lower("x")
        scanner.get().value shouldBe Token.EndInterpolatedEscape // }
        scanner.get().value shouldBe Token.EndInterpolatedString // "

        val t6 = scanner.get()
        t6.value shouldBe Token.EndOfInput
        t6.begin.value shouldBe 11

        reader.currentIndex.value shouldBe 11
        reader.peek() shouldBe Some('t')
      }
    }

    it("should report UnbalancedBraces at EOF if portal brace is missing") {
      val input = "{ x "
      TestReaderFactory.fromString(input) { (reader, lineMap) =>
        val scanner = Scanner.create(reader, IdentifierPolicy.Default, portalMode = true)

        scanner.get().value shouldBe Token.LBrace
        scanner.get().value shouldBe Token.Identifier.Lower("x")

        val t3 = scanner.get()
        t3.value shouldBe Token.Error(ScanError.UnbalancedBraces)
        // Error index should be at EOF
        t3.begin.value shouldBe 3
        t3.end.value shouldBe 3
      }
    }
  }
}

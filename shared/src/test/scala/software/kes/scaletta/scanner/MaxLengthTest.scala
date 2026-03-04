package software.kes.scaletta.scanner

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.testsupport.{AssertExpectedTokens, TestReaderFactory}

class MaxLengthTest extends AnyFunSpec with Matchers with AssertExpectedTokens {
  describe("Scanner - length constraints") {
    val smallLimit = new IdentifierPolicy {
      override def maxIdentifierLength: Option[Int] = Some(10)
    }

    it("should report IdentifierTooLong for plain identifiers exceeding the limit") {
      val input = "abcdefghijklm" // 13 chars
      TestReaderFactory.fromString(input) { reader =>
        val scanner = Scanner.create(reader, smallLimit)
        val t1 = scanner.get()
        t1.value shouldBe Token.Error(ScanError.IdentifierTooLong)
        t1.begin.value shouldBe 0
        t1.end.value shouldBe 10
      }
    }

    it("should allow identifiers exactly at the limit") {
      val input = "abcdefghij" // 10 chars
      TestReaderFactory.fromString(input) { reader =>
        val scanner = Scanner.create(reader, smallLimit)
        val t1 = scanner.get()
        t1.value shouldBe Token.Identifier.Lower("abcdefghij")
      }
    }

    it("should report IdentifierTooLong for operator identifiers exceeding the limit") {
      val input = "+++++++++++" // 11 chars
      TestReaderFactory.fromString(input) { reader =>
        val scanner = Scanner.create(reader, smallLimit)
        val t1 = scanner.get()
        t1.value shouldBe Token.Error(ScanError.IdentifierTooLong)
      }
    }

    it("should report IdentifierTooLong for quoted identifiers exceeding the limit") {
      val input = "`abcdefghijklm`" // 13 chars inside
      TestReaderFactory.fromString(input) { reader =>
        val scanner = Scanner.create(reader, smallLimit)
        val t1 = scanner.get()
        t1.value shouldBe Token.Error(ScanError.IdentifierTooLong)
      }
    }

    it("should report IdentifierTooLong for interpolated string prefixes exceeding the limit") {
      // In Scala, 's' or 'f' are typical. But the scanner supports custom names too.
      val input = "thisisalongprefix\"hello\"" // prefix is 16 chars
      TestReaderFactory.fromString(input) { reader =>
        val scanner = Scanner.create(reader, smallLimit)
        val t1 = scanner.get()
        t1.value shouldBe Token.Error(ScanError.IdentifierTooLong)
      }
    }

    it("should allow long reserved words even if they exceed maxIdentifierLength (if policy allows)") {
      // reserved words have their own max length check in IdentifierScanner which is currently hardcoded 
      // to Token.maxReservedWordLength (which is the length of the longest reserved word).
      // Let's assume there's no reserved word > 10 for this test or we use a limit smaller than any reserved word.
      val verySmallLimit = new IdentifierPolicy {
        override def maxIdentifierLength: Option[Int] = Some(2)
      }
      val input = "yield" // 5 chars
      TestReaderFactory.fromString(input) { reader =>
        val scanner = Scanner.create(reader, verySmallLimit)
        val t1 = scanner.get()
        t1.value shouldBe Token.Yield
      }
    }

    it("should handle extremely long numeric literals without crashing") {
      val longNumber = "1" + ("0" * 1000)
      TestReaderFactory.fromString(longNumber) { reader =>
        val scanner = Scanner.create(reader, IdentifierPolicy.Default)
        val t1 = scanner.get()
        // It might be an error if it's too large for BigInt/BigDecimal, 
        // but it should not crash or hang.
        t1.value match {
          case Token.IntLiteral(_) => // ok
          case Token.Error(ScanError.IntegerNumberTooLarge) => // also ok
          case other => fail(s"Unexpected token: $other")
        }
      }
    }

    it("should handle extremely long string literals without crashing") {
      val longString = "\"" + ("a" * 10000) + "\""
      TestReaderFactory.fromString(longString) { reader =>
        val scanner = Scanner.create(reader, IdentifierPolicy.Default)
        val t1 = scanner.get()
        t1.value shouldBe a[Token.StringLiteral]
      }
    }

    it("should handle extremely long comments without crashing") {
      val longComment = "/*" + ("*" * 10000) + "*/"
      val input = s"$longComment 123"
      TestReaderFactory.fromString(input) { reader =>
        val scanner = Scanner.create(reader, IdentifierPolicy.Default)
        val t1 = scanner.get()
        t1.value shouldBe Token.IntLiteral(123)
      }
    }
  }
}

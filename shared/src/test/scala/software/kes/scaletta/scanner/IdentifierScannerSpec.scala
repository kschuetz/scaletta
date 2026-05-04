package software.kes.scaletta.scanner

import org.scalactic.source.Position
import org.scalatest.BeforeAndAfter
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.common.Interpolator
import software.kes.scaletta.internal.reporting.Pos
import software.kes.scaletta.scanner.ScanError.{EmptyQuotedIdentifier, IdentifierTooLong, UnclosedQuotedIdentifier}
import software.kes.scaletta.testsupport.ScannerTestHelpers.{failure, success}
import software.kes.scaletta.testsupport.{AssertExpectedTokens, TestReaderFactory}

class IdentifierScannerSpec extends AnyFunSpec with Matchers with BeforeAndAfter with AssertExpectedTokens {
  private var policy = MyPolicy()

  before {
    policy = MyPolicy()
  }

  describe("IdentifierScanner") {
    describe("not an identifier") {
      it("empty") {
        check("")
      }

      it("numbers") {
        check("0", success(Token.IntLiteral(0), 0, 0))
        check("123", success(Token.IntLiteral(123), 0, 2))
        check("1_23", success(Token.IntLiteral(123), 0, 3))
        check("1_abc", failure(ScanError.IllegalSeparator, 1, 1),
          success(Token.Identifier.Lower("abc"), 2, 4))
      }

      it("delimiters") {
        check("'", failure(ScanError.UnclosedCharacterLiteral, 0, 0))
        check("\"", failure(ScanError.UnclosedStringLiteral, 0, 0))
        check("{", success(Token.LBrace, 0, 0))
        check("}", success(Token.RBrace, 0, 0))
        check("(", success(Token.LParen, 0, 0))
        check(")", success(Token.RParen, 0, 0))
        check("[", success(Token.LBracket, 0, 0))
        check("]", success(Token.RBracket, 0, 0))
        check(".", success(Token.Dot, 0, 0))
        check(",", success(Token.Comma, 0, 0))
        check(";", success(Token.Semicolon, 0, 0))
      }

      it("whitespace") {
        check(" ")
        check("\t")
        check("\n")
      }
    }

    describe("lowercase") {
      it("single letter") {
        check("a", success(Token.Identifier.Lower("a"), 0, 0))
        check("ä", success(Token.Identifier.Lower("ä"), 0, 0))
        check("ａ", success(Token.Identifier.Lower("ａ"), 0, 0))
      }

      it("multi-letter") {
        check("abc_123_", success(Token.Identifier.Lower("abc_123_"), 0, 7))
        check("äbc_$123", success(Token.Identifier.Lower("äbc_$123"), 0, 7))
        check("ａＢＣ", success(Token.Identifier.Lower("ａＢＣ"), 0, 2))
      }
    }

    describe("uppercase") {
      it("single letter") {
        check("A", success(Token.Identifier.Upper("A"), 0, 0))
        check("Ä", success(Token.Identifier.Upper("Ä"), 0, 0))
        check("Ａ", success(Token.Identifier.Upper("Ａ"), 0, 0))
      }
    }

    describe("$") {
      it("single letter") {
        check("$", success(Token.Identifier.Lower("$"), 0, 0))
      }

      it("multi-letter") {
        check("$$", success(Token.Identifier.Lower("$$"), 0, 1))
        check("$a", success(Token.Identifier.Lower("$a"), 0, 1))
        check("$A", success(Token.Identifier.Lower("$A"), 0, 1))
        check("$Ä", success(Token.Identifier.Lower("$Ä"), 0, 1))
        check("$_", success(Token.Identifier.Lower("$_"), 0, 1))
        check("$1", success(Token.Identifier.Lower("$1"), 0, 1))
        check("$123", success(Token.Identifier.Lower("$123"), 0, 3))
        check("$abc_123_DEF_$", success(Token.Identifier.Lower("$abc_123_DEF_$"), 0, 13))
      }
    }

    describe("_") {
      it("single letter") {
        check("_", success(Token.Underscore, 0, 0))
      }

      it("multi-letter") {
        check("_$", success(Token.Identifier.Lower("_$"), 0, 1))
        check("_a", success(Token.Identifier.Lower("_a"), 0, 1))
        check("_A", success(Token.Identifier.Lower("_A"), 0, 1))
        check("_Ä", success(Token.Identifier.Lower("_Ä"), 0, 1))
        check("__", success(Token.Identifier.Lower("__"), 0, 1))
        check("_1", success(Token.Identifier.Lower("_1"), 0, 1))
        check("_123", success(Token.Identifier.Lower("_123"), 0, 3))
        check("_abc_123_DEF_$", success(Token.Identifier.Lower("_abc_123_DEF_$"), 0, 13))
      }
    }

    describe("quoted") {
      it("valid") {
        check("`a`", success(Token.Identifier.Quoted("a"), 0, 2))
        check("`123`", success(Token.Identifier.Quoted("123"), 0, 4))
        check("`ａＢＣ äbc_$123`", success(Token.Identifier.Quoted("ａＢＣ äbc_$123"), 0, 13))
        check("` /* comment */ `", success(Token.Identifier.Quoted(" /* comment */ "), 0, 16))
        check("`There are \"quotes\" in this name`", success(Token.Identifier.Quoted("There are \"quotes\" in this name"), 0, 32))
        check("`tab:\\t backslash:\\\\`", success(Token.Identifier.Quoted("tab:\t backslash:\\"), 0, 20))
      }

      it("invalid") {
        check("``", failure(EmptyQuotedIdentifier, 0, 1))
        check("`unclosed", failure(UnclosedQuotedIdentifier, 0, 8))
        check("`newline:\n`", failure(UnclosedQuotedIdentifier, 0, 8),
          success(Token.Semicolon, 9, 9), failure(ScanError.UnclosedQuotedIdentifier, 10, 10))
      }
    }

    describe("reserved words") {
      it("should recognize all reserved words") {
        Token.allReservedWords.foreach { reservedWord =>
          val input = reservedWord.name
          withClue(reservedWord.name) {
            check(input, success(reservedWord, 0, reservedWord.name.length - 1))
          }
        }
      }
    }

    describe("operators") {
      it("single character") {
        check("!", success(Token.Identifier.Operator("!"), 0, 0))
        check("#", success(Token.Hash, 0, 0))
        check("%", success(Token.Identifier.Operator("%"), 0, 0))
        check("&", success(Token.Ampersand, 0, 0))
        check("*", success(Token.Identifier.Operator("*"), 0, 0))
        check("+", success(Token.Identifier.Operator("+"), 0, 0))
        check("-", success(Token.Identifier.Operator("-"), 0, 0))
        check("/", success(Token.Identifier.Operator("/"), 0, 0))
        check(":", success(Token.Colon, 0, 0))
        check("<", success(Token.Identifier.Operator("<"), 0, 0))
        check("=", success(Token.Eq, 0, 0))
        check(">", success(Token.Identifier.Operator(">"), 0, 0))
        check("?", success(Token.Identifier.Operator("?"), 0, 0))
        check("@", success(Token.At, 0, 0))
        check("\\", success(Token.Identifier.Operator("\\"), 0, 0))
        check("^", success(Token.Identifier.Operator("^"), 0, 0))
        check("|", success(Token.Pipe, 0, 0))
        check("~", success(Token.Identifier.Operator("~"), 0, 0))
        check("¦", success(Token.Identifier.Operator("¦"), 0, 0))
        check("©", success(Token.Identifier.Operator("©"), 0, 0))
        check("¬", success(Token.Identifier.Operator("¬"), 0, 0))
        check("®", success(Token.Identifier.Operator("®"), 0, 0))
        check("°", success(Token.Identifier.Operator("°"), 0, 0))
        check("±", success(Token.Identifier.Operator("±"), 0, 0))
        check("×", success(Token.Identifier.Operator("×"), 0, 0))
        check("÷", success(Token.Identifier.Operator("÷"), 0, 0))
        check("϶", success(Token.Identifier.Operator("϶"), 0, 0))
        check("҂", success(Token.Identifier.Operator("҂"), 0, 0))
        check("֍", success(Token.Identifier.Operator("֍"), 0, 0))
        check("֎", success(Token.Identifier.Operator("֎"), 0, 0))
        check("؆", success(Token.Identifier.Operator("؆"), 0, 0))
        check("؇", success(Token.Identifier.Operator("؇"), 0, 0))
        check("￩", success(Token.Identifier.Operator("￩"), 0, 0))
        check("￪", success(Token.Identifier.Operator("￪"), 0, 0))
        check("￫", success(Token.Identifier.Operator("￫"), 0, 0))
        check("￬", success(Token.Identifier.Operator("￬"), 0, 0))
      }

      it("multi-character") {
        check("+++", success(Token.Identifier.Operator("+++"), 0, 2))
        check("::", success(Token.Identifier.Operator("::"), 0, 1))
        check(":+@-:", success(Token.Identifier.Operator(":+@-:"), 0, 4))
      }
    }

    describe("begin interpolated string") {
      it("single line") {
        check("s\"",
          success(Token.BeginInterpolatedString(Interpolator.fromName("s")), 0, 1),
          failure(ScanError.UnclosedStringLiteral, 2, 2)
        )
        check("Abc_123_$_xyz\"",
          success(Token.BeginInterpolatedString(Interpolator.fromName("Abc_123_$_xyz")), 0, 13),
          failure(ScanError.UnclosedStringLiteral, 14, 14)
        )
      }

      it("multi-line") {
        check("s\"\"\"",
          success(Token.BeginMultiLineInterpolatedString(Interpolator.fromName("s")), 0, 3),
          failure(ScanError.UnclosedMultiLineString, 4, 4)
        )
        check("Abc_123_$_xyz\"\"\"",
          success(Token.BeginMultiLineInterpolatedString(Interpolator.fromName("Abc_123_$_xyz")), 0, 15),
          failure(ScanError.UnclosedMultiLineString, 16, 16)
        )
      }

      it("not valid identifier") {
        check("_abc\"",
          success(Token.Identifier.Lower("_abc"), 0, 3),
          failure(ScanError.UnclosedStringLiteral, 4, 4)
        )
        check("+\"",
          success(Token.Identifier.Operator("+"), 0, 0),
          failure(ScanError.UnclosedStringLiteral, 1, 1)
        )
      }
    }

    describe("with max length") {
      it("not too long") {
        policy = MyPolicy(maxIdentifierLength = Some(4))
        check("abc", success(Token.Identifier.Lower("abc"), 0, 2))
        check("abcd", success(Token.Identifier.Lower("abcd"), 0, 3))
      }

      it("too long") {
        policy = MyPolicy(maxIdentifierLength = Some(4))
        check("abcde", failure(IdentifierTooLong, 0, 4))
        check("abcdef", failure(IdentifierTooLong, 0, 5))
      }
    }

    describe("Unicode combining marks") {
      it("should include combining marks in identifiers (NFD support)") {
        // 'é' as 'e' (\u0065) followed by combining acute accent (\u0301)
        val input = "cafe\u0301"
        check(input, success(Token.Identifier.Lower("cafe\u0301"), 0, 4))
      }

      it("should not allow a combining mark to start an identifier") {
        // A combining mark by itself or at the start is not a valid identifier start.
        val input = "\u0301abc"
        check(input, failure(ScanError.InvalidCharacter, 0, 0), success(Token.Identifier.Lower("abc"), 1, 3))
      }
    }
  }

  private def check(input: String,
                    expectedTokens: Pos[Token]*)
                   (implicit pos: Position): Unit = {
    TestReaderFactory.fromString(input) { (reader, lineMap) =>
      val scanner = Scanner.create(reader, policy)
      val actualTokens = Iterator.continually(scanner.get()).takeWhile(_.value != Token.EndOfInput).toVector
      assertExpectedTokens(input, lineMap, expectedTokens.toVector, actualTokens)
    }
  }

  private case class MyPolicy(maxIdentifierLength: Option[Int] = None) extends IdentifierPolicy
}

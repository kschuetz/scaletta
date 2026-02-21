package software.kes.scaletta.scanner

import org.scalatest.BeforeAndAfter
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.scanner.ScannerError.{EmptyQuotedIdentifier, IdentifierTooLong, UnclosedQuotedIdentifier}
import software.kes.scaletta.testsupport.ScannerTestHelpers.{failure, success}
import software.kes.scaletta.testsupport.TestReaderFactory
import software.kes.scaletta.util.CharBuffer

class IdentifierScannerTest extends AnyFunSpec with Matchers with BeforeAndAfter {
  private val buffer = CharBuffer.create()
  private var policy = MyPolicy()

  before {
    policy = MyPolicy()
  }

  describe("IdentifierScanner") {
    describe("not an identifier") {
      it("empty") {
        check("", None)
      }

      it("numbers") {
        check("0", None)
        check("123", None)
        check("1_23", None)
        check("1_abc", None)
      }

      it("delimiters") {
        check("'", None)
        check("\"", None)
        check("{", None)
        check("}", None)
        check("(", None)
        check(")", None)
        check("[", None)
        check("]", None)
        check(".", None)
        check(",", None)
        check(";", None)
      }

      it("whitespace") {
        check(" ", None)
        check("\t", None)
        check("\n", None)
      }
    }

    describe("lowercase") {
      it("single letter") {
        check("a", Some(success(Token.Identifier.Lower("a"), 0, 0)))
        check("ä", Some(success(Token.Identifier.Lower("ä"), 0, 0)))
        check("ａ", Some(success(Token.Identifier.Lower("ａ"), 0, 0)))
      }

      it("multi-letter") {
        check("abc_123_", Some(success(Token.Identifier.Lower("abc_123_"), 0, 7)))
        check("äbc_$123", Some(success(Token.Identifier.Lower("äbc_$123"), 0, 7)))
        check("ａＢＣ", Some(success(Token.Identifier.Lower("ａＢＣ"), 0, 2)))
      }
    }

    describe("uppercase") {
      it("single letter") {
        check("A", Some(success(Token.Identifier.Upper("A"), 0, 0)))
        check("Ä", Some(success(Token.Identifier.Upper("Ä"), 0, 0)))
        check("Ａ", Some(success(Token.Identifier.Upper("Ａ"), 0, 0)))
      }
    }

    describe("$") {
      it("single letter") {
        check("$", Some(success(Token.Identifier.Lower("$"), 0, 0)))
      }

      it("multi-letter") {
        check("$$", Some(success(Token.Identifier.Lower("$$"), 0, 1)))
        check("$a", Some(success(Token.Identifier.Lower("$a"), 0, 1)))
        check("$A", Some(success(Token.Identifier.Lower("$A"), 0, 1)))
        check("$Ä", Some(success(Token.Identifier.Lower("$Ä"), 0, 1)))
        check("$_", Some(success(Token.Identifier.Lower("$_"), 0, 1)))
        check("$1", Some(success(Token.Identifier.Lower("$1"), 0, 1)))
        check("$123", Some(success(Token.Identifier.Lower("$123"), 0, 3)))
        check("$abc_123_DEF_$", Some(success(Token.Identifier.Lower("$abc_123_DEF_$"), 0, 13)))
      }
    }

    describe("_") {
      it("single letter") {
        check("_", Some(success(Token.Underscore, 0, 0)))
      }

      it("multi-letter") {
        check("_$", Some(success(Token.Identifier.Lower("_$"), 0, 1)))
        check("_a", Some(success(Token.Identifier.Lower("_a"), 0, 1)))
        check("_A", Some(success(Token.Identifier.Lower("_A"), 0, 1)))
        check("_Ä", Some(success(Token.Identifier.Lower("_Ä"), 0, 1)))
        check("__", Some(success(Token.Identifier.Lower("__"), 0, 1)))
        check("_1", Some(success(Token.Identifier.Lower("_1"), 0, 1)))
        check("_123", Some(success(Token.Identifier.Lower("_123"), 0, 3)))
        check("_abc_123_DEF_$", Some(success(Token.Identifier.Lower("_abc_123_DEF_$"), 0, 13)))
      }
    }

    describe("quoted") {
      it("valid") {
        check("`a`", Some(success(Token.Identifier.Quoted("a"), 0, 2)))
        check("`123`", Some(success(Token.Identifier.Quoted("123"), 0, 4)))
        check("`ａＢＣ äbc_$123`", Some(success(Token.Identifier.Quoted("ａＢＣ äbc_$123"), 0, 13)))
        check("` /* comment */ `", Some(success(Token.Identifier.Quoted(" /* comment */ "), 0, 16)))
        check("`There are \"quotes\" in this name`", Some(success(Token.Identifier.Quoted("There are \"quotes\" in this name"), 0, 32)))
        check("`tab:\\t backslash:\\\\`", Some(success(Token.Identifier.Quoted("tab:\t backslash:\\"), 0, 20)))
      }

      it("invalid") {
        check("``", Some(failure(EmptyQuotedIdentifier, 0, 1)))
        check("`unclosed", Some(failure(UnclosedQuotedIdentifier, 0, 8)))
        check("`newline:\n`", Some(failure(UnclosedQuotedIdentifier, 0, 8)))
      }
    }

    describe("reserved words") {
      it("should recognize all reserved words") {
        Token.allReservedWords.foreach { reservedWord =>
          val input = s"${reservedWord.name} $$"
          withClue(reservedWord.name) {
            check(input, Some(success(reservedWord, 0, reservedWord.name.length - 1)))
          }
        }
      }
    }

    describe("operators") {
      it("single character") {
        check("!", Some(success(Token.Identifier.Operator("!"), 0, 0)))
        check("#", Some(success(Token.Hash, 0, 0)))
        check("%", Some(success(Token.Identifier.Operator("%"), 0, 0)))
        check("&", Some(success(Token.Identifier.Operator("&"), 0, 0)))
        check("*", Some(success(Token.Identifier.Operator("*"), 0, 0)))
        check("+", Some(success(Token.Identifier.Operator("+"), 0, 0)))
        check("-", Some(success(Token.Identifier.Operator("-"), 0, 0)))
        check("/", Some(success(Token.Identifier.Operator("/"), 0, 0)))
        check(":", Some(success(Token.Colon, 0, 0)))
        check("<", Some(success(Token.Identifier.Operator("<"), 0, 0)))
        check("=", Some(success(Token.Eq, 0, 0)))
        check(">", Some(success(Token.Identifier.Operator(">"), 0, 0)))
        check("?", Some(success(Token.Identifier.Operator("?"), 0, 0)))
        check("@", Some(success(Token.At, 0, 0)))
        check("\\", Some(success(Token.Identifier.Operator("\\"), 0, 0)))
        check("^", Some(success(Token.Identifier.Operator("^"), 0, 0)))
        check("|", Some(success(Token.Identifier.Operator("|"), 0, 0)))
        check("~", Some(success(Token.Identifier.Operator("~"), 0, 0)))
        check("¦", Some(success(Token.Identifier.Operator("¦"), 0, 0)))
        check("©", Some(success(Token.Identifier.Operator("©"), 0, 0)))
        check("¬", Some(success(Token.Identifier.Operator("¬"), 0, 0)))
        check("®", Some(success(Token.Identifier.Operator("®"), 0, 0)))
        check("°", Some(success(Token.Identifier.Operator("°"), 0, 0)))
        check("±", Some(success(Token.Identifier.Operator("±"), 0, 0)))
        check("×", Some(success(Token.Identifier.Operator("×"), 0, 0)))
        check("÷", Some(success(Token.Identifier.Operator("÷"), 0, 0)))
        check("϶", Some(success(Token.Identifier.Operator("϶"), 0, 0)))
        check("҂", Some(success(Token.Identifier.Operator("҂"), 0, 0)))
        check("֍", Some(success(Token.Identifier.Operator("֍"), 0, 0)))
        check("֎", Some(success(Token.Identifier.Operator("֎"), 0, 0)))
        check("؆", Some(success(Token.Identifier.Operator("؆"), 0, 0)))
        check("؇", Some(success(Token.Identifier.Operator("؇"), 0, 0)))
        check("￩", Some(success(Token.Identifier.Operator("￩"), 0, 0)))
        check("￪", Some(success(Token.Identifier.Operator("￪"), 0, 0)))
        check("￫", Some(success(Token.Identifier.Operator("￫"), 0, 0)))
        check("￬", Some(success(Token.Identifier.Operator("￬"), 0, 0)))
      }

      it("multi-character") {
        check("+++", Some(success(Token.Identifier.Operator("+++"), 0, 2)))
        check("::", Some(success(Token.Identifier.Operator("::"), 0, 1)))
        check(":+@-:", Some(success(Token.Identifier.Operator(":+@-:"), 0, 4)))
      }
    }

    describe("begin interpolated string") {
      it("single line") {
        check("s\"", Some(success(Token.BeginInterpolatedString(Interpolator.fromName("s")), 0, 1)))
        check("Abc_123_$_xyz\"", Some(success(Token.BeginInterpolatedString(Interpolator.fromName("Abc_123_$_xyz")), 0, 13)))
      }

      it("multi-line") {
        check("s\"\"\"", Some(success(Token.BeginMultiLineInterpolatedString(Interpolator.fromName("s")), 0, 3)))
        check("Abc_123_$_xyz\"\"\"", Some(success(Token.BeginMultiLineInterpolatedString(Interpolator.fromName("Abc_123_$_xyz")), 0, 15)))
      }

      it("not valid identifier") {
        check("_abc\"", Some(success(Token.Identifier.Lower("_abc"), 0, 3)), checkRemainder = false)
        check("+\"", Some(success(Token.Identifier.Operator("+"), 0, 0)), checkRemainder = false)
      }
    }

    describe("with max length") {
      it("not too long") {
        policy = MyPolicy(maxIdentifierLength = Some(4))
        check("abc", Some(success(Token.Identifier.Lower("abc"), 0, 2)))
        check("abcd", Some(success(Token.Identifier.Lower("abcd"), 0, 3)))
      }

      it("too long") {
        policy = MyPolicy(maxIdentifierLength = Some(4))
        check("abcde", Some(failure(IdentifierTooLong, 0, 4)))
        check("abcdef", Some(failure(IdentifierTooLong, 0, 5)))
      }
    }

    describe("Unicode combining marks") {
      it("should include combining marks in identifiers (NFD support)") {
        // 'é' as 'e' (\u0065) followed by combining acute accent (\u0301)
        val input = "cafe\u0301"
        check(input, Some(success(Token.Identifier.Lower("cafe\u0301"), 0, 4)))
      }

      it("should not allow a combining mark to start an identifier") {
        // A combining mark by itself or at the start is not a valid identifier start.
        val input = "\u0301abc"
        check(input, None)
      }
    }
  }

  private def check(input: String, expected: Option[Pos[Token]],
                    checkRemainder: Boolean = true): Unit = {
    val scanner = new IdentifierScanner(policy)
    TestReaderFactory.fromString(input) { reader =>
      val result = scanner.tryScan(reader, buffer)
      withClue(input) {
        result shouldBe expected
      }
    }

    if (checkRemainder) {
      expected match {
        case Some(Pos(Token.Error(_), _, _)) => ()
        case Some(_) =>
          TestReaderFactory.fromString(input + " $") { reader =>
            val result = scanner.tryScan(reader, buffer)
            result shouldBe expected
            reader.get() shouldBe Some(' ')
            reader.get() shouldBe Some('$')
          }
        case _ => ()
      }
    }
  }

  private case class MyPolicy(maxIdentifierLength: Option[Int] = None) extends IdentifierPolicy
}

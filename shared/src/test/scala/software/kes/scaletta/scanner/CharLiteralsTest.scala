package software.kes.scaletta.scanner

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.testsupport.ScannerTestHelpers.{failure, success}
import software.kes.scaletta.testsupport.TestReaderFactory
import software.kes.scaletta.util.CharBuffer

class CharLiteralsTest extends AnyFunSpec with Matchers {
  private val buffer = CharBuffer.create()

  describe("charLiteral") {
    it("'a'") {
      TestReaderFactory.fromString("a' $") { reader =>
        Literals.charLiteral(reader) shouldBe success(Token.CharLiteral('a'), -1, 1)
        reader.get() shouldBe Some(' ')
        reader.get() shouldBe Some('$')
      }
    }

    it("space") {
      TestReaderFactory.fromString(" ' $") { reader =>
        Literals.charLiteral(reader) shouldBe success(Token.CharLiteral(' '), -1, 1)
        reader.get() shouldBe Some(' ')
        reader.get() shouldBe Some('$')
      }
    }

    it("\\n") {
      TestReaderFactory.fromString("\\n' $") { reader =>
        Literals.charLiteral(reader) shouldBe success(Token.CharLiteral('\n'), -1, 2)
        reader.get() shouldBe Some(' ')
        reader.get() shouldBe Some('$')
      }
    }

    it("\\u0041") {
      TestReaderFactory.fromString("\\u0041' $") { reader =>
        Literals.charLiteral(reader) shouldBe success(Token.CharLiteral('A'), -1, 6)
        reader.get() shouldBe Some(' ')
        reader.get() shouldBe Some('$')
      }
    }

    it("\\u21d2") {
      TestReaderFactory.fromString("\\u21d2' $") { reader =>
        Literals.charLiteral(reader) shouldBe success(Token.CharLiteral('⇒'), -1, 6)
        reader.get() shouldBe Some(' ')
        reader.get() shouldBe Some('$')
      }
    }

    it("⇒") {
      TestReaderFactory.fromString("⇒' $") { reader =>
        Literals.charLiteral(reader) shouldBe success(Token.CharLiteral('⇒'), -1, 1)
        reader.get() shouldBe Some(' ')
        reader.get() shouldBe Some('$')
      }
    }

    it("unclosed 1") {
      TestReaderFactory.fromString("a") { reader =>
        Literals.charLiteral(reader) shouldBe failure(ScannerError.UnclosedCharacterLiteral, -1, 0)
        reader.get() shouldBe None
      }
    }

    it("unclosed 2") {
      TestReaderFactory.fromString("a $") { reader =>
        Literals.charLiteral(reader) shouldBe failure(ScannerError.UnclosedCharacterLiteral, -1, 1)
        reader.get() shouldBe Some(' ')
      }
    }

    it("empty") {
      TestReaderFactory.fromString("' $") { reader =>
        Literals.charLiteral(reader) shouldBe failure(ScannerError.EmptyCharacterLiteral, -1, 0)
        reader.get() shouldBe Some(' ')
      }
    }
  }

  describe("stringLiteral") {
    describe("single-line") {
      it("empty string") {
        TestReaderFactory.fromString("\" $") { reader =>
          Literals.stringLiteral(reader, buffer) shouldBe success(Token.StringLiteral(""), -1, 0)
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }

      it("simple string, no escapes") {
        TestReaderFactory.fromString("this is a simple string\" $") { reader =>
          Literals.stringLiteral(reader, buffer) shouldBe success(Token.StringLiteral("this is a simple string"), -1, 23)
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }

      it("string with escapes") {
        TestReaderFactory.fromString(raw"this \n string \t has \f escapes \\ " + "\" $") { reader =>
          Literals.stringLiteral(reader, buffer) shouldBe success(
            Token.StringLiteral("this \n string \t has \f escapes \\ "), -1, 36)
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }

      it("string with escaped quotes") {
        TestReaderFactory.fromString("before \\\"quotes\\\" after\" $") { reader =>
          Literals.stringLiteral(reader, buffer) shouldBe success(
            Token.StringLiteral("before \"quotes\" after"), -1, 23)
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }

      it("string with unicode sequences") {
        TestReaderFactory.fromString("⇒ is the same as \\u21d2!\" $") { reader =>
          Literals.stringLiteral(reader, buffer) shouldBe success(
            Token.StringLiteral("⇒ is the same as ⇒!"), -1, 24)
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }
    }

    describe("multi-line") {
      it("empty string") {
        TestReaderFactory.fromString("\"\"\"\"\" $") { reader =>
          Literals.stringLiteral(reader, buffer) shouldBe success(Token.MultiLineString(""), -1, 4)
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }

      it("simple string, no new lines, no escapes") {
        TestReaderFactory.fromString("\"\"this is a simple string\"\"\" $") { reader =>
          Literals.stringLiteral(reader, buffer) shouldBe success(Token.MultiLineString("this is a simple string"), -1, 27)
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }

      it("string with new lines and escapes") {
        TestReaderFactory.fromString("\"\"line 1\nline 2\nline 3\\nthis\\tline\\fhas\\\\escapes \"\"\" $") { reader =>
          Literals.stringLiteral(reader, buffer) shouldBe success(
            Token.MultiLineString("line 1\nline 2\nline 3\nthis\tline\fhas\\escapes "), -1, 51)
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }

      it("string with escaped quotes") {
        TestReaderFactory.fromString("\"\"before \\\"quotes\\\" after\"\"\" $") { reader =>
          Literals.stringLiteral(reader, buffer) shouldBe success(
            Token.MultiLineString("before \"quotes\" after"), -1, 27)
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }

      it("string with unescaped quotes") {
        TestReaderFactory.fromString("\"\"before \"single\" \"\"double\"\" after\"\"\" $") { reader =>
          Literals.stringLiteral(reader, buffer) shouldBe success(
            Token.MultiLineString("before \"single\" \"\"double\"\" after"), -1, 36)
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }

      it("string with unicode sequences") {
        TestReaderFactory.fromString("\"\"⇒ is the same as \\u21d2!\"\"\" $") { reader =>
          Literals.stringLiteral(reader, buffer) shouldBe success(
            Token.MultiLineString("⇒ is the same as ⇒!"), -1, 28)
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }
    }
  }
}

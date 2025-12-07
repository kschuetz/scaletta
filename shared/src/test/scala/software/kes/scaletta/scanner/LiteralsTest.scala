package software.kes.scaletta.scanner

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.testsupport.TestReaderFactory
import software.kes.scaletta.util.CharBuffer

class LiteralsTest extends AnyFunSpec with Matchers {
  private val buffer = CharBuffer.create()

  describe("Literals") {
    describe("charLiteral") {
      describe("valid input") {
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
      }
    }

    describe("stringLiteral") {
      describe("valid input") {
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
    }
  }

  private def success[E](token: Token, begin: Int, end: Int): Pos[Either[E, Token]] =
    Pos(Right(token), CharIndex(begin), CharIndex(end))
}

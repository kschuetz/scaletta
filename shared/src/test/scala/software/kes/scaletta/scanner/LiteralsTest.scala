package software.kes.scaletta.scanner

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.testsupport.ScannerTestHelpers.{failure, success}
import software.kes.scaletta.testsupport.TestReaderFactory
import software.kes.scaletta.util.CharBuffer

class LiteralsTest extends AnyFunSpec with Matchers {
  private val buffer = CharBuffer.create()

  describe("Literals") {
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

    describe("numericLiteral") {
      it("binary int (small)") {
        TestReaderFactory.fromString("b1010_1010 $") { reader =>
          Literals.numericLiteral(negative = false, leadingDecimalPoint = false, '0', reader, buffer) shouldBe
            success(Token.IntLiteral(170), -1, 9)
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }

      it("binary long (small)") {
        TestReaderFactory.fromString("b1010_1010L $") { reader =>
          Literals.numericLiteral(negative = false, leadingDecimalPoint = false, '0', reader, buffer) shouldBe
            success(Token.LongLiteral(170), -1, 10)
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }

      it("binary (large)") {
        TestReaderFactory.fromString("b1010_1010_1010_1010_1010_1010_1010_1010_1010L $") { reader =>
          Literals.numericLiteral(negative = false, leadingDecimalPoint = false, '0', reader, buffer) shouldBe
            success(Token.LongLiteral(45812984490L), -1, 45)
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }

      it("negative binary int (small)") {
        TestReaderFactory.fromString("b1010_1010 $") { reader =>
          Literals.numericLiteral(negative = true, leadingDecimalPoint = false, '0', reader, buffer) shouldBe
            success(Token.IntLiteral(-170), -2, 9)
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }

      it("negative binary long (small)") {
        TestReaderFactory.fromString("b1010_1010L $") { reader =>
          Literals.numericLiteral(negative = true, leadingDecimalPoint = false, '0', reader, buffer) shouldBe
            success(Token.LongLiteral(-170), -2, 10)
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }

      it("negative binary (large)") {
        TestReaderFactory.fromString("b1010_1010_1010_1010_1010_1010_1010_1010_1010L $") { reader =>
          Literals.numericLiteral(negative = true, leadingDecimalPoint = false, '0', reader, buffer) shouldBe
            success(Token.LongLiteral(-45812984490L), -2, 45)
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }

      it("hex int (small)") {
        TestReaderFactory.fromString("x1234_abcd $") { reader =>
          Literals.numericLiteral(negative = false, leadingDecimalPoint = false, '0', reader, buffer) shouldBe
            success(Token.IntLiteral(0x1234abcd), -1, 9)
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }

      it("hex long (small)") {
        TestReaderFactory.fromString("x1234_ABCDL $") { reader =>
          Literals.numericLiteral(negative = false, leadingDecimalPoint = false, '0', reader, buffer) shouldBe
            success(Token.LongLiteral(0x1234abcd), -1, 10)
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }

      it("hex (large)") {
        TestReaderFactory.fromString("x1234_5678_abcd_EF01l $") { reader =>
          Literals.numericLiteral(negative = false, leadingDecimalPoint = false, '0', reader, buffer) shouldBe
            success(Token.LongLiteral(0x1234_5678_abcd_ef01L), -1, 20)
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }

      it("negative hex int (small)") {
        TestReaderFactory.fromString("x1234_AbCd $") { reader =>
          Literals.numericLiteral(negative = true, leadingDecimalPoint = false, '0', reader, buffer) shouldBe
            success(Token.IntLiteral(-0x1234abcd), -2, 9)
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }

      it("negative hex long (small)") {
        TestReaderFactory.fromString("x1234_aBcDl $") { reader =>
          Literals.numericLiteral(negative = true, leadingDecimalPoint = false, '0', reader, buffer) shouldBe
            success(Token.LongLiteral(-0x1234abcd), -2, 10)
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }

      it("negative hex (large)") {
        TestReaderFactory.fromString("x1234_5678_Abcd_eF01L $") { reader =>
          Literals.numericLiteral(negative = true, leadingDecimalPoint = false, '0', reader, buffer) shouldBe
            success(Token.LongLiteral(-0x1234_5678_abcd_ef01L), -2, 20)
          reader.get() shouldBe Some(' ')
          reader.get() shouldBe Some('$')
        }
      }
    }
  }

}

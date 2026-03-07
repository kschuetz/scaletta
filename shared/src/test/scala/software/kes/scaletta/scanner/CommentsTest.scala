package software.kes.scaletta.scanner

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.reporting.{CharIndex, ColumnIndex, LineIndex, Position}
import software.kes.scaletta.scanner.CommentResult.{BlockComment, LineComment, NoComment, Unterminated}
import software.kes.scaletta.testsupport.LineEndingInterpolators._
import software.kes.scaletta.testsupport.TestReaderFactory

class CommentsTest extends AnyFunSpec with Matchers {
  describe("comments") {
    describe("not a comment") {
      it("case 1") {
        TestReaderFactory.fromString("") { (reader, lineMap) =>
          Comments.scanComments(reader) shouldBe NoComment
          reader.get() shouldBe None
        }
      }
      it("case 2") {
        TestReaderFactory.fromString("not a comment") { (reader, lineMap) =>
          Comments.scanComments(reader) shouldBe NoComment
          reader.get() shouldBe Some('n')
          reader.get() shouldBe Some('o')
        }
      }
      it("case 3") {
        TestReaderFactory.fromString("/not a comment") { (reader, lineMap) =>
          Comments.scanComments(reader) shouldBe NoComment
          reader.get() shouldBe Some('/')
          reader.get() shouldBe Some('n')
        }
      }
    }
    describe("line comments") {
      it("case 1") {
        TestReaderFactory.fromString("//\n$") { (reader, lineMap) =>
          Comments.scanComments(reader) shouldBe LineComment(Some(CharIndex(2)))
          reader.get() shouldBe Some('\n')
          reader.get() shouldBe Some('$')
        }
      }
      it("case 2") {
        TestReaderFactory.fromString("// line comment\n$") { (reader, lineMap) =>
          Comments.scanComments(reader) shouldBe LineComment(Some(CharIndex(15)))
          reader.get() shouldBe Some('\n')
          reader.get() shouldBe Some('$')
        }
      }
      it("case 3") {
        TestReaderFactory.fromString("/// line comment\n$") { (reader, lineMap) =>
          Comments.scanComments(reader) shouldBe LineComment(Some(CharIndex(16)))
          reader.get() shouldBe Some('\n')
          reader.get() shouldBe Some('$')
        }
      }
      it("case 4") {
        TestReaderFactory.fromString("/// /*line comment*/ \n$") { (reader, lineMap) =>
          Comments.scanComments(reader) shouldBe LineComment(Some(CharIndex(21)))
          reader.get() shouldBe Some('\n')
          reader.get() shouldBe Some('$')
        }
      }

      describe("line ending invariance") {
        it("LF (\\n)") {
          TestReaderFactory.fromString(lf"// comment\n$$") { (reader, lineMap) =>
            Comments.scanComments(reader) shouldBe LineComment(Some(CharIndex(10)))
            reader.get() shouldBe Some('\n')
            reader.get() shouldBe Some('$')
            reader.lineMap.indexToPosition(CharIndex(11)) shouldBe Position(LineIndex(2), ColumnIndex(1))
          }
        }
        it("CR (\\r)") {
          TestReaderFactory.fromString(cr"// comment\n$$") { (reader, lineMap) =>
            Comments.scanComments(reader) shouldBe LineComment(Some(CharIndex(10)))
            reader.get() shouldBe Some('\r')
            reader.get() shouldBe Some('$')
            reader.lineMap.indexToPosition(CharIndex(11)) shouldBe Position(LineIndex(2), ColumnIndex(1))
          }
        }
        it("CRLF (\\r\\n)") {
          TestReaderFactory.fromString(crlf"// comment\n$$") { (reader, lineMap) =>
            Comments.scanComments(reader) shouldBe LineComment(Some(CharIndex(10)))
            reader.get() shouldBe Some('\r')
            reader.get() shouldBe Some('\n')
            reader.get() shouldBe Some('$')
            reader.lineMap.indexToPosition(CharIndex(12)) shouldBe Position(LineIndex(2), ColumnIndex(1))
          }
        }
      }

      it("at EOF (no newline)") {
        TestReaderFactory.fromString("// comment") { (reader, lineMap) =>
          Comments.scanComments(reader) shouldBe LineComment(None)
          reader.get() shouldBe None
        }
      }
    }
    describe("block comments") {
      describe("single-line") {
        it("simple") {
          TestReaderFactory.fromString("/* single-line * block comment */$") { (reader, lineMap) =>
            Comments.scanComments(reader) shouldBe BlockComment.SingleLine
            reader.get() shouldBe Some('$')
          }
        }

        it("nested") {
          TestReaderFactory.fromString("/* single-line * /* nested */ block comment */$ */") { (reader, lineMap) =>
            Comments.scanComments(reader) shouldBe BlockComment.SingleLine
            reader.get() shouldBe Some('$')
          }
        }

        it("unterminated simple") {
          TestReaderFactory.fromString("/* single-line * unterminated") { (reader, lineMap) =>
            Comments.scanComments(reader) shouldBe Unterminated
            reader.get() shouldBe None
          }
        }

        it("unterminated nested") {
          TestReaderFactory.fromString("/* single-line * /* nested */ unterminated") { (reader, lineMap) =>
            Comments.scanComments(reader) shouldBe Unterminated
            reader.get() shouldBe None
          }
        }
      }

      describe("multi-line") {
        it("simple") {
          TestReaderFactory.fromString(
            lf"""/* multi-line
                 block comment */$$""") { (reader, _) =>
            Comments.scanComments(reader) shouldBe BlockComment.MultiLine(CharIndex(13))
            reader.get() shouldBe Some('$')
          }
        }

        it("nested") {
          TestReaderFactory.fromString(
            lf"""/* multi-line *
                 /* nested */
                 block comment */$$ */""") { (reader, _) =>
            Comments.scanComments(reader) shouldBe BlockComment.MultiLine(CharIndex(45))
            reader.get() shouldBe Some('$')
          }
        }

        describe("line ending invariance") {
          it("LF (\\n)") {
            TestReaderFactory.fromString(lf"/* line 1\nline 2 */$$") { (reader, lineMap) =>
              Comments.scanComments(reader) shouldBe BlockComment.MultiLine(CharIndex(9))
              reader.get() shouldBe Some('$')
              reader.lineMap.indexToPosition(CharIndex(10)) shouldBe Position(LineIndex(2), ColumnIndex(1))
            }
          }
          it("CR (\\r)") {
            TestReaderFactory.fromString(cr"/* line 1\nline 2 */$$") { (reader, lineMap) =>
              Comments.scanComments(reader) shouldBe BlockComment.MultiLine(CharIndex(9))
              reader.get() shouldBe Some('$')
              reader.lineMap.indexToPosition(CharIndex(10)) shouldBe Position(LineIndex(2), ColumnIndex(1))
            }
          }
          it("CRLF (\\r\\n)") {
            TestReaderFactory.fromString(crlf"/* line 1\nline 2 */$$") { (reader, lineMap) =>
              Comments.scanComments(reader) shouldBe BlockComment.MultiLine(CharIndex(9))
              reader.get() shouldBe Some('$')
              reader.lineMap.indexToPosition(CharIndex(11)) shouldBe Position(LineIndex(2), ColumnIndex(1))
            }
          }
        }

        it("unterminated simple") {
          TestReaderFactory.fromString(
            lf"""/* multi-line *
                 block comment $$""") { (reader, _) =>
            Comments.scanComments(reader) shouldBe Unterminated
            reader.get() shouldBe None
          }
        }

        it("unterminated nested") {
          TestReaderFactory.fromString(
            lf"""/* multi-line *
                 /* nested */
             block comment $$""") { (reader, _) =>
            Comments.scanComments(reader) shouldBe Unterminated
            reader.get() shouldBe None
          }
        }
      }
    }
  }
}

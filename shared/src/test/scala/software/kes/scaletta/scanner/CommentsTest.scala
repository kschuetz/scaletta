package software.kes.scaletta.scanner

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.scanner.CommentResult.{BlockComment, LineComment, NoComment, Unterminated}
import software.kes.scaletta.testsupport.TestReaderFactory

class CommentsTest extends AnyFunSpec with Matchers {
  describe("comments") {
    describe("not a comment") {
      it("case 1") {
        TestReaderFactory.fromString("") { reader =>
          Comments.scanComments(reader) shouldBe NoComment
          reader.get() shouldBe None
        }
      }
      it("case 2") {
        TestReaderFactory.fromString("not a comment") { reader =>
          Comments.scanComments(reader) shouldBe NoComment
          reader.get() shouldBe Some('n')
          reader.get() shouldBe Some('o')
        }
      }
      it("case 3") {
        TestReaderFactory.fromString("/not a comment") { reader =>
          Comments.scanComments(reader) shouldBe NoComment
          reader.get() shouldBe Some('/')
          reader.get() shouldBe Some('n')
        }
      }
    }
    describe("line comments") {
      it("case 1") {
        TestReaderFactory.fromString("//\n$") { reader =>
          Comments.scanComments(reader) shouldBe LineComment(CharIndex(2))
          reader.get() shouldBe Some('\n')
          reader.get() shouldBe Some('$')
        }
      }
      it("case 2") {
        TestReaderFactory.fromString("// line comment\n$") { reader =>
          Comments.scanComments(reader) shouldBe LineComment(CharIndex(15))
          reader.get() shouldBe Some('\n')
          reader.get() shouldBe Some('$')
        }
      }
      it("case 3") {
        TestReaderFactory.fromString("/// line comment\n$") { reader =>
          Comments.scanComments(reader) shouldBe LineComment(CharIndex(16))
          reader.get() shouldBe Some('\n')
          reader.get() shouldBe Some('$')
        }
      }
      it("case 4") {
        TestReaderFactory.fromString("/// /*line comment*/ \n$") { reader =>
          Comments.scanComments(reader) shouldBe LineComment(CharIndex(21))
          reader.get() shouldBe Some('\n')
          reader.get() shouldBe Some('$')
        }
      }
    }
    describe("block comments") {
      describe("single-line") {
        it("simple") {
          TestReaderFactory.fromString("/* single-line * block comment */$") { reader =>
            Comments.scanComments(reader) shouldBe BlockComment.SingleLine
            reader.get() shouldBe Some('$')
          }
        }

        it("nested") {
          TestReaderFactory.fromString("/* single-line * /* nested */ block comment */$ */") { reader =>
            Comments.scanComments(reader) shouldBe BlockComment.SingleLine
            reader.get() shouldBe Some('$')
          }
        }

        it("unterminated simple") {
          TestReaderFactory.fromString("/* single-line * unterminated") { reader =>
            Comments.scanComments(reader) shouldBe Unterminated
            reader.get() shouldBe None
          }
        }

        it("unterminated nested") {
          TestReaderFactory.fromString("/* single-line * /* nested */ unterminated") { reader =>
            Comments.scanComments(reader) shouldBe Unterminated
            reader.get() shouldBe None
          }
        }
      }

      describe("multi-line") {
        it("simple") {
          TestReaderFactory.fromString(
            """/* multi-line
             block comment */$""") { reader =>
            Comments.scanComments(reader) shouldBe BlockComment.MultiLine
            reader.get() shouldBe Some('$')
          }
        }

        it("nested") {
          TestReaderFactory.fromString(
            """/* multi-line *
               /* nested */
             block comment */$ */""") { reader =>
            Comments.scanComments(reader) shouldBe BlockComment.MultiLine
            reader.get() shouldBe Some('$')
          }
        }

        it("unterminated simple") {
          TestReaderFactory.fromString(
            """/* multi-line *
             block comment $""") { reader =>
            Comments.scanComments(reader) shouldBe Unterminated
            reader.get() shouldBe None
          }
        }

        it("unterminated nested") {
          TestReaderFactory.fromString(
            """/* multi-line *
               /* nested */
             block comment $""") { reader =>
            Comments.scanComments(reader) shouldBe Unterminated
            reader.get() shouldBe None
          }
        }
      }
    }
  }
}

package software.kes.scaletta.scanner

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.reporting.{CharIndex, Pos}

class TokenBufferSpec extends AnyFunSpec with Matchers {
  private def posToken(token: Token, begin: Int, end: Int): Pos[Token] =
    Pos(token, CharIndex(begin), CharIndex(end))

  private val token1 = posToken(Token.Val, 0, 2)
  private val token2 = posToken(Token.Identifier.Lower("x"), 4, 4)
  private val eofToken = posToken(Token.EndOfInput, 10, 10)

  describe("TokenBuffer") {
    describe("basic lifecycle and operations") {
      it("should be empty initially") {
        val buffer = TokenBuffer.create()
        buffer.isEmpty shouldBe true
        buffer.isExhausted shouldBe false
        buffer.length shouldBe 0
      }

      it("should handle enqueue and dequeue") {
        val buffer = TokenBuffer.create()
        buffer.enqueue(token1)
        buffer.isEmpty shouldBe false
        buffer.length shouldBe 1
        buffer.dequeue() shouldBe token1
        buffer.isEmpty shouldBe true
        buffer.length shouldBe 0
      }

      it("should maintain FIFO order") {
        val buffer = TokenBuffer.create()
        buffer.enqueue(token1)
        buffer.enqueue(token2)
        buffer.length shouldBe 2
        buffer.dequeue() shouldBe token1
        buffer.dequeue() shouldBe token2
      }

      it("should provide mostRecentlyAdded in normal state") {
        val buffer = TokenBuffer.create()
        buffer.mostRecentlyAdded shouldBe None
        buffer.enqueue(token1)
        buffer.mostRecentlyAdded shouldBe Some(token1)
        buffer.enqueue(token2)
        buffer.mostRecentlyAdded shouldBe Some(token2)
        buffer.dequeue()
        buffer.mostRecentlyAdded shouldBe Some(token2)
        buffer.dequeue()
        buffer.mostRecentlyAdded shouldBe None
      }
    }

    describe("lookahead via get(index)") {
      it("should allow valid access within queue") {
        val buffer = TokenBuffer.create()
        buffer.enqueue(token1)
        buffer.enqueue(token2)
        buffer.get(0) shouldBe token1
        buffer.get(1) shouldBe token2
      }

      it("should throw IndexOutOfBoundsException when out of bounds in normal state") {
        val buffer = TokenBuffer.create()
        intercept[IndexOutOfBoundsException] {
          buffer.get(0)
        }
        buffer.enqueue(token1)
        intercept[IndexOutOfBoundsException] {
          buffer.get(1)
        }
      }
    }

    describe("termination logic") {
      it("should transition to exhausted after terminate") {
        val buffer = TokenBuffer.create()
        buffer.terminate(eofToken)
        buffer.isExhausted shouldBe true
        buffer.isEmpty shouldBe false // Not empty because _terminalToken is set
      }

      it("should ignore subsequent terminate calls (idempotency)") {
        val buffer = TokenBuffer.create()
        val otherEof = posToken(Token.EndOfInput, 99, 99)
        buffer.terminate(eofToken)
        buffer.terminate(otherEof)
        buffer.dequeue() shouldBe eofToken
      }

      it("should ignore enqueues after termination") {
        val buffer = TokenBuffer.create()
        buffer.terminate(eofToken)
        buffer.enqueue(token1)
        buffer.length shouldBe 0
        buffer.dequeue() shouldBe eofToken
      }

      it("should ignore enqueues after termination even if queue is not empty") {
        val buffer = TokenBuffer.create()
        buffer.enqueue(token1)
        buffer.terminate(eofToken)
        buffer.enqueue(token2)
        buffer.length shouldBe 1
        buffer.get(0) shouldBe token1
        buffer.get(1) shouldBe eofToken
      }
    }

    describe("infinite EOF protocol") {
      it("should return terminal token indefinitely from dequeue when queue is drained") {
        val buffer = TokenBuffer.create()
        buffer.enqueue(token1)
        buffer.terminate(eofToken)
        buffer.dequeue() shouldBe token1
        buffer.dequeue() shouldBe eofToken
        buffer.dequeue() shouldBe eofToken
        buffer.dequeue() shouldBe eofToken
      }

      it("should return terminal token for lookahead beyond queue when terminated") {
        val buffer = TokenBuffer.create()
        buffer.terminate(eofToken)
        buffer.get(0) shouldBe eofToken
        buffer.get(1) shouldBe eofToken
        buffer.get(100) shouldBe eofToken
      }

      it("should return terminal token for lookahead beyond queue even if queue has items") {
        val buffer = TokenBuffer.create()
        buffer.enqueue(token1)
        buffer.terminate(eofToken)
        buffer.get(0) shouldBe token1
        buffer.get(1) shouldBe eofToken
        buffer.get(10) shouldBe eofToken
      }

      it("should provide mostRecentlyAdded in terminal state") {
        val buffer = TokenBuffer.create()
        buffer.terminate(eofToken)
        buffer.mostRecentlyAdded shouldBe Some(eofToken)

        val buffer2 = TokenBuffer.create()
        buffer2.enqueue(token1)
        buffer2.terminate(eofToken)
        buffer2.mostRecentlyAdded shouldBe Some(token1)
        buffer2.dequeue()
        buffer2.mostRecentlyAdded shouldBe Some(eofToken)
      }
    }

    describe("error conditions") {
      it("should throw NoSuchElementException when dequeueing from empty non-terminated buffer") {
        val buffer = TokenBuffer.create()
        intercept[NoSuchElementException] {
          buffer.dequeue()
        }
      }

      it("should throw IndexOutOfBoundsException when get(0) on empty non-terminated buffer") {
        val buffer = TokenBuffer.create()
        intercept[IndexOutOfBoundsException] {
          buffer.get(0)
        }
      }
    }
  }
}

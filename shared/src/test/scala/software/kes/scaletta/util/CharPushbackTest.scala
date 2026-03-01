package software.kes.scaletta.util

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class CharPushbackTest extends AnyFunSpec with Matchers {
  describe("CharPushback") {
    it("should handle basic push and pop (single width)") {
      val pb = CharPushback.create(16)
      pb.push('a')
      pb.push('b')
      pb.nonEmpty shouldBe true
      pb.peek() shouldBe 'b'
      pb.peekDoubleWidth() shouldBe false
      pb.pop() shouldBe 'b'
      pb.peek() shouldBe 'a'
      pb.peekDoubleWidth() shouldBe false
      pb.pop() shouldBe 'a'
      pb.isEmpty shouldBe true
    }

    it("should handle double-width characters") {
      val pb = CharPushback.create(16)
      pb.push('\n', isDoubleWidth = true)
      pb.peek() shouldBe '\n'
      pb.peekDoubleWidth() shouldBe true
      pb.pop() shouldBe '\n'
      pb.isEmpty shouldBe true
    }

    it("should correctly mix single and double width characters") {
      val pb = CharPushback.create(16)
      pb.push('a', isDoubleWidth = false)
      pb.push('\n', isDoubleWidth = true)
      pb.push('b', isDoubleWidth = false)

      pb.peekDoubleWidth() shouldBe false
      pb.pop() shouldBe 'b'

      pb.peekDoubleWidth() shouldBe true
      pb.pop() shouldBe '\n'

      pb.peekDoubleWidth() shouldBe false
      pb.pop() shouldBe 'a'

      pb.isEmpty shouldBe true
    }

    it("should handle push(String)") {
      val pb = CharPushback.create(16)
      pb.push("abc")
      // push("abc") pushes 'c', then 'b', then 'a' (reversed)
      // So peek/pop should return 'a', 'b', 'c'
      pb.pop() shouldBe 'a'
      pb.pop() shouldBe 'b'
      pb.pop() shouldBe 'c'
      pb.isEmpty shouldBe true
    }

    it("should grow the buffer when needed") {
      val initialCapacity = 8
      val pb = CharPushback.create(initialCapacity)
      for (i <- 0 until 100) {
        pb.push((i % 128).toChar, isDoubleWidth = (i % 2 == 0))
      }

      for (i <- (0 until 100).reverse) {
        pb.peekDoubleWidth() shouldBe (i % 2 == 0)
        pb.pop() shouldBe (i % 128).toChar
      }
      pb.isEmpty shouldBe true
    }

    it("should correctly manage width flags across multiple Longs") {
      // Each Long stores 64 flags. Let's push 150 characters to span 3 Longs.
      val pb = CharPushback.create(64)
      for (i <- 0 until 150) {
        // Set double width for even indices
        pb.push('x', isDoubleWidth = (i % 2 == 0))
      }

      for (i <- (0 until 150).reverse) {
        pb.peekDoubleWidth() shouldBe (i % 2 == 0)
        pb.pop() shouldBe 'x'
      }
      pb.isEmpty shouldBe true
    }

    it("should reset correctly") {
      val pb = CharPushback.create(16)
      pb.push('a', isDoubleWidth = true)
      pb.reset()
      pb.isEmpty shouldBe true
      pb.push('b', isDoubleWidth = false)
      pb.peekDoubleWidth() shouldBe false
      pb.pop() shouldBe 'b'
    }

    it("should handle initialCapacity of 0 or negative") {
      val pb0 = CharPushback.create(0)
      pb0.isEmpty shouldBe true
      pb0.push('a')
      pb0.pop() shouldBe 'a'

      val pbNeg = CharPushback.create(-1)
      pbNeg.isEmpty shouldBe true
      pbNeg.push('b')
      pbNeg.pop() shouldBe 'b'
    }

    it("should handle peek on empty buffer") {
      val pb = CharPushback.create(16)
      pb.peek() shouldBe 0.toChar
      pb.peekDoubleWidth() shouldBe false
    }
  }
}

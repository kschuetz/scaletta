package software.kes.scaletta.util

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class CharPushbackSpec extends AnyFunSpec with Matchers {
  describe("CharPushback") {
    it("should handle basic push and pop") {
      val pb = CharPushback.create(16)
      pb.push('a')
      pb.push('b')
      pb.nonEmpty shouldBe true
      pb.peek() shouldBe 'b'
      pb.pop() shouldBe 'b'
      pb.peek() shouldBe 'a'
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
        pb.push((i % 128).toChar)
      }

      for (i <- (0 until 100).reverse) {
        pb.pop() shouldBe (i % 128).toChar
      }
      pb.isEmpty shouldBe true
    }

    it("should reset correctly") {
      val pb = CharPushback.create(16)
      pb.push('a')
      pb.reset()
      pb.isEmpty shouldBe true
      pb.push('b')
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
    }
  }
}

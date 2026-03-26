package software.kes.scaletta.util

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class CharBufferSpec extends AnyFunSpec with Matchers {

  describe("CharBuffer") {
    it("should be created with create") {
      val cb = CharBuffer.create(43)
      cb.size shouldBe 0
      cb.capacity shouldBe 43
      cb.isEmpty shouldBe true
      cb.nonEmpty shouldBe false
    }

    it("should have a default initial capacity") {
      val cb = CharBuffer.create()
      cb.capacity shouldBe 256
    }

    it("should write a character") {
      val cb = CharBuffer.create(10)
      cb.write('A')
      cb.size shouldBe 1
      cb.isEmpty shouldBe false
      cb.nonEmpty shouldBe true
      cb.charAtIndex(0) shouldBe 'A'
    }

    it("should ignore zero character in write") {
      val cb = CharBuffer.create(10)
      cb.write(0.toChar)
      cb.size shouldBe 0
    }

    it("should write an iterable of characters") {
      val cb = CharBuffer.create(10)
      cb.write("Hello")
      cb.size shouldBe 5
      cb.slice() shouldBe "Hello"
    }

    it("should grow when capacity is exceeded") {
      val cb = CharBuffer.create(2)
      cb.write('A')
      cb.write('B')
      val initialCapacity = cb.capacity
      cb.write('C')
      cb.size shouldBe 3
      cb.capacity should be > initialCapacity
      cb.slice() shouldBe "ABC"
    }

    it("should return correct firstChar and lastChar") {
      val cb = CharBuffer.create(10)
      cb.firstChar shouldBe 0.toChar
      cb.lastChar shouldBe 0.toChar

      cb.write('A')
      cb.firstChar shouldBe 'A'
      cb.lastChar shouldBe 'A'

      cb.write('B')
      cb.firstChar shouldBe 'A'
      cb.lastChar shouldBe 'B'
    }

    it("should mark the current pointer") {
      val cb = CharBuffer.create(10)
      cb.write('A')
      cb.mark() shouldBe 1
      cb.write('B')
      cb.mark() shouldBe 2
    }

    it("should chop the last character") {
      val cb = CharBuffer.create(10)
      cb.write('A')
      cb.write('B')
      cb.chop() shouldBe 'B'
      cb.size shouldBe 1
      cb.lastChar shouldBe 'A'
    }

    it("should reset the buffer") {
      val cb = CharBuffer.create(10)
      cb.write("Hello")
      cb.reset()
      cb.size shouldBe 0
      cb.isEmpty shouldBe true
    }

    it("should return character at specific index") {
      val cb = CharBuffer.create(10)
      cb.write("ABC")
      cb.charAtIndex(0) shouldBe 'A'
      cb.charAtIndex(1) shouldBe 'B'
      cb.charAtIndex(2) shouldBe 'C'
    }

    it("should truncate the buffer") {
      val cb = CharBuffer.create(10)
      cb.write("Hello")
      cb.truncate(3)
      cb.size shouldBe 3
      cb.slice() shouldBe "Hel"

      cb.truncate(10) // should not grow or change size if greater than current size
      cb.size shouldBe 3

      cb.truncate(0)
      cb.size shouldBe 0
    }

    it("should return slices of the buffer") {
      val cb = CharBuffer.create(10)
      cb.write("Hello World")
      cb.slice(0, 5) shouldBe "Hello"
      cb.slice(6, 11) shouldBe "World"
      cb.slice(5) shouldBe "Hello"
      cb.slice() shouldBe "Hello World"
    }

    it("should handle slice end > size") {
      val cb = CharBuffer.create(10)
      cb.write("Hi")
      cb.slice(0, 10) shouldBe "Hi"
    }

    it("should insert a character at an index") {
      val cb = CharBuffer.create(10)
      cb.write("AC")
      cb.insert(1, 'B')
      cb.slice() shouldBe "ABC"
      cb.size shouldBe 3

      cb.insert(0, 'X')
      cb.slice() shouldBe "XABC"
    }

    it("should grow during insert") {
      val cb = CharBuffer.create(2)
      cb.write('A')
      cb.write('B')
      cb.insert(1, 'X')
      cb.slice() shouldBe "AXB"
      cb.size shouldBe 3
    }
  }
}

package software.kes.scaletta.util.conversions

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class CollectionToTupleSpec extends AnyFunSpec with Matchers {

  describe("CollectionToTuple") {
    describe("arrayToTuple") {
      (2 to 22).foreach { arity =>
        it(s"should convert an array of size $arity to a Tuple$arity") {
          val elements = (1 to arity).map(i => i * 43).toArray
          val result = CollectionToTuple.arrayToTuple(elements)

          result shouldBe a[Product]
          val product = result.asInstanceOf[Product]
          product.productArity shouldBe arity
          (0 until arity).foreach { i =>
            product.productElement(i) shouldBe elements(i)
          }
        }
      }

      it("should truncate an array of size 23 to a Tuple22") {
        val elements = (1 to 23).map(i => i * 43).toArray
        val result = CollectionToTuple.arrayToTuple(elements)

        result shouldBe a[Product]
        val product = result.asInstanceOf[Product]
        product.productArity shouldBe 22
        (0 until 22).foreach { i =>
          product.productElement(i) shouldBe elements(i)
        }
      }

      it("should return Unit for an empty array") {
        CollectionToTuple.arrayToTuple(Array.empty[Int]) shouldBe (())
      }

      it("should return the element directly for an array of size 1") {
        CollectionToTuple.arrayToTuple(Array(41)) shouldBe 41
      }
    }
  }
}

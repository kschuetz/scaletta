package software.kes.scaletta.util.reflection

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class PrimitivesSpec extends AnyFunSpec with Matchers {
  describe("Primitives") {
    describe("isAnyVal") {
      it("should return true for Boolean") {
        Primitives.isAnyVal(true) shouldBe true
        Primitives.isAnyVal(false) shouldBe true
        Primitives.isAnyVal(java.lang.Boolean.TRUE) shouldBe true
      }

      it("should return true for Byte") {
        Primitives.isAnyVal(41.toByte) shouldBe true
        Primitives.isAnyVal(java.lang.Byte.valueOf(43.toByte)) shouldBe true
      }

      it("should return true for Short") {
        Primitives.isAnyVal(47.toShort) shouldBe true
        Primitives.isAnyVal(java.lang.Short.valueOf(53.toShort)) shouldBe true
      }

      it("should return true for Int") {
        Primitives.isAnyVal(59) shouldBe true
        Primitives.isAnyVal(java.lang.Integer.valueOf(61)) shouldBe true
      }

      it("should return true for Long") {
        Primitives.isAnyVal(67L) shouldBe true
        Primitives.isAnyVal(java.lang.Long.valueOf(71L)) shouldBe true
      }

      it("should return true for Char") {
        Primitives.isAnyVal('A') shouldBe true
        Primitives.isAnyVal(java.lang.Character.valueOf('B')) shouldBe true
      }

      it("should return true for Float") {
        Primitives.isAnyVal(73.0f) shouldBe true
        Primitives.isAnyVal(java.lang.Float.valueOf(79.0f)) shouldBe true
      }

      it("should return true for Double") {
        Primitives.isAnyVal(83.0) shouldBe true
        Primitives.isAnyVal(java.lang.Double.valueOf(89.0)) shouldBe true
      }

      it("should return true for Unit") {
        Primitives.isAnyVal(()) shouldBe true
        Primitives.isAnyVal(scala.runtime.BoxedUnit.UNIT) shouldBe true
      }

      it("should return false for null") {
        Primitives.isAnyVal(null) shouldBe false
      }

      it("should return false for reference types") {
        Primitives.isAnyVal("string") shouldBe false
        Primitives.isAnyVal(List(1, 2, 3)) shouldBe false
        Primitives.isAnyVal(Some(41)) shouldBe false
        Primitives.isAnyVal(new Object()) shouldBe false
      }
    }
  }
}

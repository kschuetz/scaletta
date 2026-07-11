package software.kes.scaletta.util.conversions

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ObjectToPrimitiveSpec extends AnyFunSpec with Matchers {
  describe("ObjectToPrimitive") {
    describe("objectToBoolean") {
      it("should convert java.lang.Boolean to Boolean") {
        ObjectToPrimitive.objectToBoolean(java.lang.Boolean.TRUE) shouldBe true
        ObjectToPrimitive.objectToBoolean(java.lang.Boolean.FALSE) shouldBe false
      }

      it("should return false for other types") {
        ObjectToPrimitive.objectToBoolean(java.lang.Integer.valueOf(1)) shouldBe false
        ObjectToPrimitive.objectToBoolean(java.lang.Character.valueOf('t')) shouldBe false
        ObjectToPrimitive.objectToBoolean(null) shouldBe false
      }
    }

    describe("objectToInt") {
      it("should convert java.lang.Number to Int") {
        ObjectToPrimitive.objectToInt(java.lang.Integer.valueOf(41)) shouldBe 41
        ObjectToPrimitive.objectToInt(java.lang.Long.valueOf(43L)) shouldBe 43
        ObjectToPrimitive.objectToInt(java.lang.Float.valueOf(47.5f)) shouldBe 47
        ObjectToPrimitive.objectToInt(java.lang.Double.valueOf(53.9)) shouldBe 53
      }

      it("should convert java.lang.Character to Int") {
        ObjectToPrimitive.objectToInt(java.lang.Character.valueOf('A')) shouldBe 65
      }

      it("should convert java.lang.Boolean to Int") {
        ObjectToPrimitive.objectToInt(java.lang.Boolean.TRUE) shouldBe 1
        ObjectToPrimitive.objectToInt(java.lang.Boolean.FALSE) shouldBe 0
      }

      it("should return 0 for other types") {
        ObjectToPrimitive.objectToInt("string") shouldBe 0
        ObjectToPrimitive.objectToInt(null) shouldBe 0
      }
    }

    describe("objectToLong") {
      it("should convert java.lang.Number to Long") {
        ObjectToPrimitive.objectToLong(java.lang.Long.valueOf(41L)) shouldBe 41L
        ObjectToPrimitive.objectToLong(java.lang.Integer.valueOf(43)) shouldBe 43L
        ObjectToPrimitive.objectToLong(java.lang.Double.valueOf(47.8)) shouldBe 47L
      }

      it("should convert java.lang.Character to Long") {
        ObjectToPrimitive.objectToLong(java.lang.Character.valueOf('\u0041')) shouldBe 65L
      }

      it("should convert java.lang.Boolean to Long") {
        ObjectToPrimitive.objectToLong(java.lang.Boolean.TRUE) shouldBe 1L
        ObjectToPrimitive.objectToLong(java.lang.Boolean.FALSE) shouldBe 0L
      }

      it("should return 0 for other types") {
        ObjectToPrimitive.objectToLong(null) shouldBe 0L
      }
    }

    describe("objectToShort") {
      it("should convert java.lang.Number to Short") {
        ObjectToPrimitive.objectToShort(java.lang.Short.valueOf(41.toShort)) shouldBe 41.toShort
        ObjectToPrimitive.objectToShort(java.lang.Integer.valueOf(43)) shouldBe 43.toShort
      }

      it("should convert java.lang.Character to Short") {
        ObjectToPrimitive.objectToShort(java.lang.Character.valueOf('A')) shouldBe 65.toShort
      }

      it("should convert java.lang.Boolean to Short") {
        ObjectToPrimitive.objectToShort(java.lang.Boolean.TRUE) shouldBe 1.toShort
        ObjectToPrimitive.objectToShort(java.lang.Boolean.FALSE) shouldBe 0.toShort
      }

      it("should return 0 for other types") {
        ObjectToPrimitive.objectToShort(null) shouldBe 0.toShort
      }
    }

    describe("objectToByte") {
      it("should convert java.lang.Number to Byte") {
        ObjectToPrimitive.objectToByte(java.lang.Byte.valueOf(41.toByte)) shouldBe 41.toByte
        ObjectToPrimitive.objectToByte(java.lang.Integer.valueOf(43)) shouldBe 43.toByte
      }

      it("should convert java.lang.Character to Byte") {
        ObjectToPrimitive.objectToByte(java.lang.Character.valueOf('A')) shouldBe 65.toByte
      }

      it("should convert java.lang.Boolean to Byte") {
        ObjectToPrimitive.objectToByte(java.lang.Boolean.TRUE) shouldBe 1.toByte
        ObjectToPrimitive.objectToByte(java.lang.Boolean.FALSE) shouldBe 0.toByte
      }

      it("should return 0 for other types") {
        ObjectToPrimitive.objectToByte(null) shouldBe 0.toByte
      }
    }

    describe("objectToChar") {
      it("should convert java.lang.Number to Char") {
        ObjectToPrimitive.objectToChar(java.lang.Integer.valueOf(65)) shouldBe 'A'
        ObjectToPrimitive.objectToChar(java.lang.Double.valueOf(66.7)) shouldBe 'B'
      }

      it("should convert java.lang.Character to Char") {
        ObjectToPrimitive.objectToChar(java.lang.Character.valueOf('C')) shouldBe 'C'
      }

      it("should convert java.lang.Boolean to Char") {
        ObjectToPrimitive.objectToChar(java.lang.Boolean.TRUE) shouldBe 1.toChar
        ObjectToPrimitive.objectToChar(java.lang.Boolean.FALSE) shouldBe 0.toChar
      }

      it("should return 0 for other types") {
        ObjectToPrimitive.objectToChar(null) shouldBe 0.toChar
      }
    }

    describe("objectToDouble") {
      it("should convert java.lang.Number to Double") {
        ObjectToPrimitive.objectToDouble(java.lang.Double.valueOf(41.41)) shouldBe 41.41 +- 0.0001
        ObjectToPrimitive.objectToDouble(java.lang.Integer.valueOf(43)) shouldBe 43.0 +- 0.0001
      }

      it("should convert java.lang.Character to Double") {
        ObjectToPrimitive.objectToDouble(java.lang.Character.valueOf('A')) shouldBe 65.0 +- 0.0001
      }

      it("should convert java.lang.Boolean to Double") {
        ObjectToPrimitive.objectToDouble(java.lang.Boolean.TRUE) shouldBe 1.0 +- 0.0001
        ObjectToPrimitive.objectToDouble(java.lang.Boolean.FALSE) shouldBe 0.0 +- 0.0001
      }

      it("should return 0 for other types") {
        ObjectToPrimitive.objectToDouble(null) shouldBe 0.0 +- 0.0001
      }
    }

    describe("objectToFloat") {
      it("should convert java.lang.Number to Float") {
        ObjectToPrimitive.objectToFloat(java.lang.Float.valueOf(41.41f)) shouldBe 41.41f +- 0.0001f
        ObjectToPrimitive.objectToFloat(java.lang.Integer.valueOf(43)) shouldBe 43.0f +- 0.0001f
      }

      it("should convert java.lang.Character to Float") {
        ObjectToPrimitive.objectToFloat(java.lang.Character.valueOf('A')) shouldBe 65.0f +- 0.0001f
      }

      it("should convert java.lang.Boolean to Float") {
        ObjectToPrimitive.objectToFloat(java.lang.Boolean.TRUE) shouldBe 1.0f +- 0.0001f
        ObjectToPrimitive.objectToFloat(java.lang.Boolean.FALSE) shouldBe 0.0f +- 0.0001f
      }

      it("should return 0 for other types") {
        ObjectToPrimitive.objectToFloat(null) shouldBe 0.0f +- 0.0001f
      }
    }
  }
}

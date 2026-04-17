package software.kes.scaletta.interpreter

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ConstantPoolBuilderSpec extends AnyFunSpec with Matchers {
  describe("ConstantPoolBuilder") {
    it("should intern Long values") {
      val builder = ConstantPoolBuilder.create()
      val idx1 = builder.internLong(41L)
      val idx2 = builder.internLong(43L)
      val idx3 = builder.internLong(41L)

      idx1 shouldBe 0
      idx2 shouldBe 1
      idx3 shouldBe 0

      val pool = builder.build()
      pool.getLong(0) shouldBe 41L
      pool.getLong(1) shouldBe 43L
    }

    it("should intern Double values") {
      val builder = ConstantPoolBuilder.create()
      val idx1 = builder.internDouble(3.14)
      val idx2 = builder.internDouble(2.71)
      val idx3 = builder.internDouble(3.14)

      idx1 shouldBe 0
      idx2 shouldBe 1
      idx3 shouldBe 0

      val pool = builder.build()
      pool.getDouble(0) shouldBe 3.14
      pool.getDouble(1) shouldBe 2.71
    }

    it("should intern Float values") {
      val builder = ConstantPoolBuilder.create()
      val idx1 = builder.internFloat(1.1f)
      val idx2 = builder.internFloat(2.2f)
      val idx3 = builder.internFloat(1.1f)

      idx1 shouldBe 0
      idx2 shouldBe 1
      idx3 shouldBe 0

      val pool = builder.build()
      pool.getFloat(0) shouldBe 1.1f
      pool.getFloat(1) shouldBe 2.2f
    }

    it("should intern String values but allow duplicate non-String object values") {
      val builder = ConstantPoolBuilder.create()
      val s1 = "hello"
      val s2 = "world"
      val s3 = "hello"

      val idx1 = builder.internObject(s1)
      val idx2 = builder.internObject(s2)
      val idx3 = builder.internObject(s3)

      idx1 shouldBe 0
      idx2 shouldBe 1
      idx3 shouldBe 0 // Interned

      val obj1 = List(1)
      val obj2 = List(1)
      val idx4 = builder.internObject(obj1)
      val idx5 = builder.internObject(obj2)

      idx4 shouldBe 2
      idx5 shouldBe 3 // Not interned

      val pool = builder.build()
      pool.getObject(0) shouldBe "hello"
      pool.getObject(1) shouldBe "world"
      pool.getObject(2) shouldBe List(1)
      pool.getObject(3) shouldBe List(1)
    }

    it("should keep different types separate") {
      val builder = ConstantPoolBuilder.create()
      builder.internLong(41L) shouldBe 0
      builder.internDouble(41.0) shouldBe 0
      builder.internFloat(41.0f) shouldBe 0
      builder.internObject("41") shouldBe 0

      val pool = builder.build()
      pool.getLong(0) shouldBe 41L
      pool.getDouble(0) shouldBe 41.0
      pool.getFloat(0) shouldBe 41.0f
      pool.getObject(0) shouldBe "41"
    }
  }
}

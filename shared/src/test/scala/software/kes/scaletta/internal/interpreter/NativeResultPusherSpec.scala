package software.kes.scaletta.internal.interpreter

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.common.BasicTypes

class NativeResultPusherSpec extends AnyFunSpec with Matchers {
  private val isJS = System.getProperty("java.vm.name") == "Scala.js"

  describe("NativeResultPusher") {
    val stack = OperandStack.create()

    def testSuccess(basicType: Byte, value: Any): Unit = {
      stack.clear()
      NativeResultPusher.pushReturn(basicType, value, stack)
      stack.size() shouldBe 1
      stack.peek shouldBe Some(value)
      stack.peekBasicType shouldBe Some(basicType)
    }

    def testFailure(basicType: Byte, value: Any): Unit = {
      stack.clear()
      an[IllegalArgumentException] should be thrownBy {
        NativeResultPusher.pushReturn(basicType, value, stack)
      }
      stack.isEmpty shouldBe true
    }

    it("should push Boolean correctly") {
      testSuccess(BasicTypes.Boolean, true)
      testSuccess(BasicTypes.Boolean, false)
      testFailure(BasicTypes.Boolean, 41)
      testFailure(BasicTypes.Boolean, "true")
      testFailure(BasicTypes.Boolean, null)
    }

    it("should push Int correctly") {
      testSuccess(BasicTypes.Int, 41)
      testFailure(BasicTypes.Int, 41L)
      if (!isJS) {
        testFailure(BasicTypes.Int, 41.0)
      }
      testFailure(BasicTypes.Int, "41")
      testFailure(BasicTypes.Int, null)
    }

    it("should push Long correctly") {
      testSuccess(BasicTypes.Long, 43L)
      testFailure(BasicTypes.Long, 43)
      testFailure(BasicTypes.Long, null)
    }

    it("should push Short correctly") {
      testSuccess(BasicTypes.Short, 41.toShort)
      if (!isJS) {
        testFailure(BasicTypes.Short, 41)
      }
      testFailure(BasicTypes.Short, null)
    }

    it("should push Byte correctly") {
      testSuccess(BasicTypes.Byte, 43.toByte)
      if (!isJS) {
        testFailure(BasicTypes.Byte, 43)
      }
      testFailure(BasicTypes.Byte, null)
    }

    it("should push Char correctly") {
      testSuccess(BasicTypes.Char, 'A')
      testFailure(BasicTypes.Char, "A")
      testFailure(BasicTypes.Char, null)
    }

    it("should push Double correctly") {
      testSuccess(BasicTypes.Double, 41.5)
      if (!isJS) {
        testFailure(BasicTypes.Double, 41.5f)
      }
      testFailure(BasicTypes.Double, null)
    }

    it("should push Float correctly") {
      testSuccess(BasicTypes.Float, 43.5f)
      if (!isJS) {
        testFailure(BasicTypes.Float, 43.5)
      }
      testFailure(BasicTypes.Float, null)
    }

    it("should push Object correctly") {
      val obj = new Object()
      testSuccess(BasicTypes.Object, obj)
      testSuccess(BasicTypes.Object, "hello")
      testSuccess(BasicTypes.Object, null)
      testSuccess(BasicTypes.Object, Integer.valueOf(41))
    }

    it("should throw descriptive IllegalArgumentException on type mismatch") {
      val stack = OperandStack.create()
      val ex = the[IllegalArgumentException] thrownBy {
        NativeResultPusher.pushReturn(BasicTypes.Int, "not an int", stack)
      }
      ex.getMessage should include("Type mismatch")
      ex.getMessage should include("Int")
      ex.getMessage should include("java.lang.String")
    }
  }
}

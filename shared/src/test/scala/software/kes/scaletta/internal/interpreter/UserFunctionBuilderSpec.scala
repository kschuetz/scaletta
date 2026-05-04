package software.kes.scaletta.internal.interpreter

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.runtime.{CoreTypes, FrameSignature, VarSpaceSignature}

class UserFunctionBuilderSpec extends AnyFunSpec with Matchers {
  describe("UserFunctionBuilder") {
    it("should write and advance") {
      val sig = VarSpaceSignature.of(FrameSignature.of(CoreTypes.IntT))
      val builder = UserFunctionBuilder.create(sig)
      builder.currentAddress shouldBe 0

      builder.writeAndAdvance(101)
      builder.currentAddress shouldBe 1
      builder.writeAndAdvance(103)
      builder.currentAddress shouldBe 2

      val func = builder.build()
      func.instructions shouldBe Seq(101, 103)
      func.varSpaceSignature shouldBe sig
    }

    it("should allow writing to an existing address") {
      val sig = VarSpaceSignature.empty
      val builder = UserFunctionBuilder.create(sig)
      builder.writeAndAdvance(10)
      builder.writeAndAdvance(20)

      builder.write(0, 15)
      val func = builder.build()
      func.instructions shouldBe Seq(15, 20)
    }

    it("should throw exception when writing to an out-of-bounds address") {
      val sig = VarSpaceSignature.empty
      val builder = UserFunctionBuilder.create(sig)
      builder.writeAndAdvance(10)

      an[IndexOutOfBoundsException] should be thrownBy {
        builder.write(-1, 0)
      }
      an[IndexOutOfBoundsException] should be thrownBy {
        builder.write(1, 0)
      }
      an[IndexOutOfBoundsException] should be thrownBy {
        builder.write(2, 0)
      }
    }

    it("should grow the buffer when needed") {
      val sig = VarSpaceSignature.empty
      val builder = UserFunctionBuilder.create(sig)
      // Initial capacity is 16, so let's write 17 elements
      for (i <- 0 until 17) {
        builder.writeAndAdvance(i)
      }

      builder.currentAddress shouldBe 17
      val func = builder.build()
      func.instructions.length shouldBe 17
      for (i <- 0 until 17) {
        func.instructions(i) shouldBe i
      }
    }

    it("should support bitmasking in write") {
      val sig = VarSpaceSignature.empty
      val builder = UserFunctionBuilder.create(sig)
      builder.writeAndAdvance(0x12345678)
      builder.write(0, 0x0000ABCD, 0x0000FFFF)
      val func = builder.build()
      func.instructions(0) shouldBe 0x1234ABCD
    }
  }
}

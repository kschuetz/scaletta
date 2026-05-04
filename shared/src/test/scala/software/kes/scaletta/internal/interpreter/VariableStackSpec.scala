package software.kes.scaletta.internal.interpreter

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.internal.runtime.{CoreTypes, FrameSignature}
import software.kes.scaletta.types.{Type, TypeId}

class VariableStackSpec extends AnyFunSpec with Matchers {
  describe("VariableStack") {
    it("should correctly expand and contract a frame") {
      val stack = VariableStack.create()
      val types: Seq[Type[TypeId]] = Seq(CoreTypes.IntT, CoreTypes.BooleanT, CoreTypes.IntT, CoreTypes.AnyRefT)
      val signature = FrameSignature.fromSeq(types)

      signature.intCount shouldBe 2
      signature.booleanCount shouldBe 1
      signature.objectCount shouldBe 1
      signature.floatCount shouldBe 0

      stack.expandFrame(signature)

      stack.ints.size() shouldBe 2
      stack.booleans.size() shouldBe 1
      stack.objects.size() shouldBe 1
      stack.floats.size() shouldBe 0

      stack.contractFrame(signature)

      stack.ints.size() shouldBe 0
      stack.booleans.size() shouldBe 0
      stack.objects.size() shouldBe 0
      stack.floats.size() shouldBe 0
    }

    it("should correctly clear all internal stacks") {
      val stack = VariableStack.create()
      val signature = FrameSignature.of(CoreTypes.IntT)
      stack.expandFrame(signature)
      stack.ints.unsafeWrite(0, 41)

      stack.clear()

      stack.ints.size() shouldBe 0
    }
  }
}
